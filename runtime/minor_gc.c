/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*              Damien Doligez, projet Para, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <string.h>
#include "caml/custom.h"
#include "caml/config.h"
#include "caml/fail.h"
#include "caml/finalise.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/signals.h"
#include "caml/weak.h"

/* Pointers into the minor heap.
   [caml_young_base]
       The [malloc] block that contains the heap.
   [caml_young_start] ... [caml_young_end]
       The whole range of the minor heap: all young blocks are inside
       this interval.
   [caml_young_semispace_boundary]
       The midpoint between [young_start] and [young_end].
   [caml_young_semispace_cur]
       Which semispace is currently used for allocation. Switched at
       the end of each minor collection.
   [caml_young_aging_ratio]
       How much of the recently-allocated memory the minor GC will
       retain in the minor heap. Expressed as a number between 0 and 1.
       0 for none, 1 for all blocks in the current semispace.
   [caml_young_alloc_start]...[caml_young_alloc_end]
       The allocation arena: newly-allocated blocks are carved from
       this interval, starting at [young_alloc_end].
   [caml_young_alloc_mid] is the mid-point of this interval.
   [caml_young_ptr]
       This is where the next allocation will take place. Note that this
       pointer may be slightly outside the allocation arena, if the GC is
       called when an allocation fails for lack of space.
   [caml_young_trigger], [caml_young_limit]
       These pointers are inside the allocation arena.
       - [young_trigger] is how far we can allocate before
         triggering [caml_gc_dispatch]. Currently, it is either
         [young_alloc_start] or [young_alloc_mid].
       - [young_limit] is the pointer that is compared to
         [young_ptr] for allocation. It is either
         [caml_young_alloc_end] if a signal is pending and we are in
         native code, or [caml_young_trigger].
*/

/* Asserts that a word is a valid header for a young object */
#define CAMLassert_young_header(hd)                \
  CAMLassert(Wosize_hd(hd) > 0 &&                  \
             Wosize_hd(hd) <= Max_young_wosize &&  \
             (Is_white_hd(hd) || Is_black_hd (hd)))

struct generic_table CAML_TABLE_STRUCT(char);

asize_t caml_minor_heap_wsz;
static void *caml_young_base = NULL;
static value *caml_young_stack = NULL;
CAMLexport value *caml_young_start = NULL, *caml_young_end = NULL;
CAMLexport value *caml_young_alloc_start = NULL,
                 *caml_young_alloc_mid = NULL,
                 *caml_young_alloc_end = NULL;
static value *caml_young_semispace_boundary;
static uintnat caml_young_semispace_cur;
CAMLexport double caml_young_aging_ratio;
CAMLexport double caml_latest_aging_ratio;
CAMLexport value *caml_young_ptr = NULL, *caml_young_limit = NULL;
CAMLexport value *caml_young_trigger = NULL;

CAMLexport struct caml_ref_table
  caml_ref_table = { NULL, NULL, NULL, NULL, NULL, 0, 0};
CAMLexport struct caml_ref_table
  caml_ref_table_aux = { NULL, NULL, NULL, NULL, NULL, 0, 0};

CAMLexport struct caml_ephe_ref_table
  caml_ephe_ref_table = { NULL, NULL, NULL, NULL, NULL, 0, 0};

CAMLexport struct caml_custom_table
  caml_custom_table = { NULL, NULL, NULL, NULL, NULL, 0, 0};
/* Table of custom blocks in the minor heap that contain finalizers
   or GC speed parameters. */

int caml_in_minor_collection = 0;

double caml_extra_heap_resources_minor = 0;

/* [sz] and [rsv] are numbers of entries */
static void alloc_generic_table (struct generic_table *tbl, asize_t sz,
                                 asize_t rsv, asize_t element_size)
{
  void *new_table;

  tbl->size = sz;
  tbl->reserve = rsv;
  new_table = (void *) caml_stat_alloc_noexc((tbl->size + tbl->reserve) *
                                             element_size);
  if (new_table == NULL) caml_fatal_error ("not enough memory");
  if (tbl->base != NULL) caml_stat_free (tbl->base);
  tbl->base = new_table;
  tbl->ptr = tbl->base;
  tbl->threshold = tbl->base + tbl->size * element_size;
  tbl->limit = tbl->threshold;
  tbl->end = tbl->base + (tbl->size + tbl->reserve) * element_size;
}

void caml_alloc_table (struct caml_ref_table *tbl, asize_t sz, asize_t rsv)
{
  alloc_generic_table ((struct generic_table *) tbl, sz, rsv, sizeof (value *));
}

void caml_alloc_ephe_table (struct caml_ephe_ref_table *tbl, asize_t sz,
                            asize_t rsv)
{
  alloc_generic_table ((struct generic_table *) tbl, sz, rsv,
                       sizeof (struct caml_ephe_ref_elt));
}

void caml_alloc_custom_table (struct caml_custom_table *tbl, asize_t sz,
                              asize_t rsv)
{
  alloc_generic_table ((struct generic_table *) tbl, sz, rsv,
                       sizeof (struct caml_custom_elt));
}

static void reset_table (struct generic_table *tbl)
{
  tbl->size = 0;
  tbl->reserve = 0;
  if (tbl->base != NULL) caml_stat_free (tbl->base);
  tbl->base = tbl->ptr = tbl->threshold = tbl->limit = tbl->end = NULL;
}

/* Remove all elements from the table except the ones located before [keep]. */
static void clear_table (struct generic_table *tbl, char *keep)
{
  if (tbl->base == NULL){
    CAMLassert (tbl->ptr == NULL);
    CAMLassert (tbl->threshold == NULL);
    CAMLassert (tbl->limit == NULL);
    CAMLassert (tbl->end == NULL);
    CAMLassert (keep == NULL);
  }else{
    CAMLassert (keep <= tbl->ptr);
    tbl->ptr = keep;
    if (keep < tbl->threshold){
      tbl->limit = tbl->threshold;
    }
    CAMLassert (keep < tbl->limit);
  }
}

/* [bsz] is the size (in bytes) of each allocation arena (i.e. half
   the actual minor heap size). */
void caml_set_minor_heap_size (asize_t bsz)
{
  char *new_heap, *new_stack;
  void *new_heap_base;

  CAMLassert (bsz >= Bsize_wsize(Minor_heap_min));
  CAMLassert (bsz <= Bsize_wsize(Minor_heap_max));
  CAMLassert (bsz % sizeof (value) == 0);
  if (caml_young_ptr != caml_young_alloc_end
      || caml_latest_aging_ratio != 0.){
    /* We must empty the minor heap. */
    CAML_INSTR_INT ("force_minor/set_minor_heap_size@", 1);
    caml_requested_minor_gc = 0;
    caml_empty_minor_heap (0.);
  }
  CAMLassert (caml_young_ptr == caml_young_alloc_end);
  new_heap = caml_stat_alloc_aligned_noexc(2 * bsz, 0, &new_heap_base);
  if (new_heap == NULL) caml_raise_out_of_memory();
  new_stack =
    caml_stat_alloc_noexc (Bsize_wsize (Wsize_bsize (2 * bsz)
                                        / Whsize_wosize (2)));
    /* This stack needs to store at most one pointer for each block of
       size >= 2 in the minor heap. */
  if (new_stack == NULL){
    caml_stat_free (new_heap);
    caml_raise_out_of_memory ();
  }
  if (caml_page_table_add(In_young, new_heap, new_heap + 2 * bsz) != 0)
    caml_raise_out_of_memory();

  if (caml_young_start != NULL){
    caml_page_table_remove(In_young, caml_young_start, caml_young_end);
    caml_stat_free (caml_young_base);
    CAMLassert (caml_young_stack != NULL);
    caml_stat_free (caml_young_stack);
  }
  caml_young_base = new_heap_base;
  caml_young_start = (value *) new_heap;
  caml_young_end = (value *) (new_heap + 2 * bsz);
  caml_young_semispace_boundary =
    caml_young_start + Wsize_bsize (bsz);
  caml_young_semispace_cur = 0;

  caml_young_alloc_start = caml_young_start;
  caml_young_alloc_end = caml_young_semispace_boundary;
  caml_young_alloc_mid =
    caml_young_alloc_start
    + (caml_young_alloc_end - caml_young_alloc_start) / 2;
  caml_young_trigger = caml_young_alloc_start;
  caml_young_limit = caml_young_trigger;
  caml_young_ptr = caml_young_alloc_end;
  caml_minor_heap_wsz = Wsize_bsize (bsz);
  caml_young_stack = (value *) new_stack;

  reset_table ((struct generic_table *) &caml_ref_table);
  reset_table ((struct generic_table *) &caml_ephe_ref_table);
  reset_table ((struct generic_table *) &caml_custom_table);
}

static value *oldify_stack_ptr = NULL;

void caml_oldify_init (void)
{
  oldify_stack_ptr = caml_young_stack;
}

static value * aging_limit;

/* Note that the tests on the tag depend on the fact that Infix_tag,
   Forward_tag, and No_scan_tag are contiguous. */

static void oldify_one_aux (value v, value *p, int add_to_ref)
{
  value result;
  header_t hd;
  mlsize_t sz, i;
  tag_t tag;

 tail_call:
  if (Is_block (v) && Is_young (v)){
    CAMLassert (!((value *) Hp_val (v) >= caml_young_alloc_start
                  && (value *) Hp_val (v) < caml_young_ptr));
    hd = Hd_val (v);
    if (hd == 0){         /* If already forwarded */
      *p = Field (v, 0);  /*  then forward pointer is first field. */
    }else{
      CAMLassert_young_header (hd);
      tag = Tag_hd (hd);
      if (tag < Infix_tag){
        value field0;
        sz = Wosize_hd (hd);
        if ((value *) Hp_val (v) >= caml_young_alloc_start
            && (value *) Hp_val (v) < aging_limit){
          CAMLassert ((value *) Hp_val (v) >= caml_young_ptr);
          /* This block stays in the minor heap. */
          if (add_to_ref){
            /* This is a new old-to-young pointer */
            add_to_ref_table (&caml_ref_table, p);
          }
          *p = v;
          if (Is_white_hd (hd)){
            Hd_val (v) = Blackhd_hd (hd);
            if (sz > 1){
              *oldify_stack_ptr++ = v;
            }else{
              CAMLassert (sz == 1);
              p = &Field (v, 0);
              v = Field (v, 0);
              add_to_ref = 0;
              goto tail_call;
            }
          }else{
            CAMLassert (Is_black_hd (hd));
          }
        }else{
          result = caml_alloc_shr_preserving_profinfo (sz, tag, hd);
          *p = result;
          field0 = Field (v, 0);
          Hd_val (v) = 0;            /* Set forward flag */
          Field (v, 0) = result;     /*  and forward pointer. */
          if (sz > 1){
            Field (result, 0) = field0;
            *oldify_stack_ptr++ = v;
          }else{
            CAMLassert (sz == 1);
            p = &Field (result, 0);
            v = field0;
            add_to_ref = 1;
            goto tail_call;
          }
        }
      }else if (tag >= No_scan_tag){
        sz = Wosize_hd (hd);
        if ((value *) Hp_val (v) >= caml_young_alloc_start
            && (value *) Hp_val (v) < aging_limit){
          CAMLassert ((value *) Hp_val (v) >= caml_young_ptr);
          /* This block stays in the minor heap. */
          if (add_to_ref){
            /* This is a new old-to-young pointer */
            add_to_ref_table (&caml_ref_table, p);
          }
          Hd_val (v) = Blackhd_hd (hd);
          *p = v;
        }else{
          result = caml_alloc_shr_preserving_profinfo (sz, tag, hd);
          for (i = 0; i < sz; i++) Field (result, i) = Field (v, i);
          Hd_val (v) = 0;            /* Set forward flag */
          Field (v, 0) = result;     /*  and forward pointer. */
          *p = result;
        }
      }else if (tag == Infix_tag){
        mlsize_t offset = Infix_offset_hd (hd);
        caml_oldify_one (v - offset, p);   /* Cannot recurse deeper than 1. */
        *p += offset;
      }else{
        value f = Forward_val (v);
        tag_t ft = 0;
        int vv = 1;

        CAMLassert (tag == Forward_tag);
        if (Is_block (f)){
          if (Is_young (f)){
            vv = 1;
            ft = Tag_val (Hd_val (f) == 0 ? Field (f, 0) : f);
          }else{
            vv = Is_in_value_area(f);
            if (vv){
              ft = Tag_val (f);
            }
          }
        }
        if (!vv || ft == Forward_tag || ft == Lazy_tag
#ifdef FLAT_FLOAT_ARRAY
            || ft == Double_tag
#endif
            ){
          /* Do not short-circuit the pointer.  Copy as a normal block. */
          CAMLassert (Wosize_hd (hd) == 1);
          if ((value *) Hp_val (v) >= caml_young_alloc_start
              && (value *) Hp_val (v) < aging_limit){
            CAMLassert ((value *) Hp_val (v) >= caml_young_ptr);
            /* This block stays in the minor heap. */
            if (add_to_ref){
              /* This is a new old-to-young pointer */
              add_to_ref_table (&caml_ref_table, p);
            }
            Hd_val (v) = Blackhd_hd (hd);
            *p = v;
            p = &Field (v, 0);
            v = f;
            add_to_ref = 0;
            goto tail_call;
          }else{
            result = caml_alloc_shr_preserving_profinfo (1, Forward_tag, hd);
            *p = result;
            Hd_val (v) = 0;             /* Set (GC) forward flag */
            Field (v, 0) = result;      /*  and forward pointer. */
            p = &Field (result, 0);
            v = f;
            add_to_ref = 1;
            goto tail_call;
          }
        }else{
          v = f;                        /* Follow the forwarding */
          goto tail_call;               /*  then oldify. */
        }
      }
    }
  }else{
    *p = v;
  }
}

/* External version for root scanning, etc. This will never create a new
   old-to-young reference.
*/
void caml_oldify_one (value v, value *p)
{
  oldify_one_aux (v, p, 0);
}

/* Test if the ephemeron is alive, everything outside minor heap is alive */
static inline int ephe_check_alive_data(struct caml_ephe_ref_elt *re){
  mlsize_t i;
  value child;
  for (i = CAML_EPHE_FIRST_KEY; i < Wosize_val(re->ephe); i++){
    child = Field (re->ephe, i);
    if(child != caml_ephe_none
       && Is_block (child) && Is_young_and_dead (child)){
      /* Value not copied to major heap and not retained in minor heap. */
      return 0;
    }
  }
  return 1;
}

/* Finish the work that was put off by [oldify_one_aux].
   Note that [oldify_one_aux] itself is called by [caml_oldify_mopup], so we
   have to be careful to remove the top of the stack before
   oldifying its fields. */
void caml_oldify_mopup (void)
{
  value v, new_v, f;
  mlsize_t i;
  struct caml_ephe_ref_elt *re;
  int redo = 1;
  header_t hd;

  while (redo){
    redo = 0;
    while (oldify_stack_ptr != caml_young_stack){
      v = *--oldify_stack_ptr;             /* Get the head. */
      hd = Hd_val (v);
      if (hd == 0){
        /* Promoted to the major heap. */
        new_v = Field (v, 0);                /* Follow forward pointer. */
        hd = Hd_val (new_v);
        CAMLassert_young_header (hd);
        CAMLassert (Tag_hd (hd) < Infix_tag);

        f = Field (new_v, 0);
        if (Is_block (f) && Is_young (f)){
          oldify_one_aux (f, &Field (new_v, 0), 1);
        }
        for (i = 1; i < Wosize_hd (hd); i++){
          f = Field (v, i);
          if (Is_block (f) && Is_young (f)){
            oldify_one_aux (f, &Field (new_v, i), 1);
          }else{
            Field (new_v, i) = f;
          }
        }
      }else{
        /* Kept in the minor heap. */
        CAMLassert_young_header (hd);
        CAMLassert (Is_black_hd (hd));
        for (i = 0; i < Wosize_hd (hd); i++){
          f = Field (v, i);
          if (Is_block (f) && Is_young (f)){
            oldify_one_aux (f, &Field (v, i), 0);
          }
        }
      }
    }

    /* Oldify the data in the minor heap of alive ephemeron
       During minor collection keys outside the minor heap are considered
       alive */
    for (re = caml_ephe_ref_table.base;
         re < caml_ephe_ref_table.ptr; re++){
      /* look only at ephemeron with data in the minor heap */
      if (re->offset == 1){
        value *data = &Field(re->ephe,1);
        if (*data != caml_ephe_none && Is_block (*data) && Is_young (*data)){
          if (Hd_val (*data) == 0){ /* Value copied to major heap */
            *data = Field (*data, 0);
          }else if (Kept_in_minor_heap (*data)){
            CAMLassert ((value *) Hp_val (*data) >= caml_young_ptr);
            /* Stays in minor heap. */
          } else {
            if (ephe_check_alive_data(re)){
              oldify_one_aux (*data, data, 0);
              redo = 1; /* young_stack can still be empty */
            }
          }
        }
      }
    }
  }
}

/* Do a partial collection of the minor heap.
   [aging_ratio] specified how much of the most recently allocated data
   should be kept in the minor heap. It must be between 0 and 1.

   If you need to empty the minor heap, call this function with
   aging_ratio = 0.
*/
void caml_empty_minor_heap (double aging_ratio)
{
  value **r;
  struct caml_custom_elt *elt, *keep_elt;
  uintnat prev_alloc_words;
  struct caml_ephe_ref_elt *re, *keep_re;
  struct caml_ref_table old_ref_table;

  CAMLassert (aging_ratio >= 0. && aging_ratio <= 1.);
  caml_latest_aging_ratio = aging_ratio;
  if (caml_minor_gc_begin_hook != NULL) (*caml_minor_gc_begin_hook) ();
  CAML_INSTR_SETUP (tmr, "minor");
  prev_alloc_words = caml_allocated_words;
  caml_in_minor_collection = 1;
  caml_gc_message (0x02, "<");
  caml_oldify_init ();

  /* Switch to a new ref_table. */
  CAMLassert (caml_ref_table_aux.base == NULL);
  old_ref_table = caml_ref_table;
  caml_ref_table = caml_ref_table_aux;
  caml_ref_table_aux = old_ref_table;

  aging_limit = caml_young_alloc_start;
  caml_oldify_minor_long_lived_roots ();
  CAML_INSTR_TIME (tmr, "minor/long_lived_roots");

  aging_limit =
    caml_young_alloc_start
    + (uintnat)((caml_young_alloc_end - caml_young_alloc_start)
                * aging_ratio);
  CAMLassert (aging_limit <= caml_young_alloc_end);

  for (r = old_ref_table.base; r < old_ref_table.ptr; r++) {
    oldify_one_aux (**r, *r, 1);
  }
  /* Empty the old remembered set to prepare for next cycle. */
  clear_table ((struct generic_table *) &caml_ref_table_aux,
               (char *) caml_ref_table_aux.base);
  CAML_INSTR_TIME (tmr, "minor/ref_table");
  caml_oldify_minor_short_lived_roots ();
  CAML_INSTR_TIME (tmr, "minor/short_lived_roots");
  caml_oldify_mopup ();
  CAML_INSTR_TIME (tmr, "minor/copy");
  /* Update the ephemerons */
  for (re = keep_re = caml_ephe_ref_table.base;
       re < caml_ephe_ref_table.ptr; re++){
    if(re->offset < Wosize_val(re->ephe)){
      /* If it is not the case, the ephemeron has been truncated */
      value *key = &Field(re->ephe,re->offset);
      value v = *key;
      if (v != caml_ephe_none && Is_block (v) && Is_young (v)){
        if (Hd_val (v) == 0){ /* Value copied to major heap */
          *key = Field (v, 0);
        }else if (Kept_in_minor_heap (v)){
          CAMLassert ((value *) Hp_val (v) >= caml_young_ptr);
          /* Value stays in the minor heap */
          *keep_re++ = *re;
        }else{ /* Value is dead */
          CAMLassert(!ephe_check_alive_data(re));
          *key = caml_ephe_none;
          Field(re->ephe,1) = caml_ephe_none;
        }
      }
    }
  }
  /* Update the OCaml [finalise_last] values */
  caml_final_update_minor_roots_last();
  /* Run custom block finalisation of dead minor values */
  for (elt = keep_elt = caml_custom_table.base;
       elt < caml_custom_table.ptr; elt++){
    value v = elt->block;
    if (Hd_val (v) == 0){
      /* Block was copied to the major heap: adjust GC speed numbers. */
      caml_adjust_gc_speed(elt->mem, elt->max);
    }else if (Kept_in_minor_heap (v)){
      CAMLassert ((value *) Hp_val (v) >= caml_young_ptr);
      /* Block remains in the minor heap: keep its entry. */
      CAMLassert (Tag_val (v) == Custom_tag);
      *keep_elt++ = *elt;
    }else{
      /* Block will be freed: call finalization function, if any. */
      void (*final_fun)(value);
      CAMLassert (Tag_val (v) == Custom_tag);
      final_fun = Custom_ops_val(v)->finalize;
      if (final_fun != NULL) final_fun(v);
    }
  }
  CAML_INSTR_TIME (tmr, "minor/update_weak");
  caml_stat_minor_words += caml_young_alloc_end - caml_young_ptr;
  caml_gc_clock += (double) (caml_young_alloc_end - caml_young_ptr)
                   / caml_minor_heap_wsz;
  caml_young_ptr = caml_young_alloc_end;
  /* switch semispaces */
  if (caml_young_semispace_cur == 0){
    caml_young_semispace_cur = 1;
    caml_young_alloc_start = caml_young_semispace_boundary;
    caml_young_alloc_end = caml_young_end;
  }else{
    CAMLassert (caml_young_semispace_cur == 1);
    caml_young_semispace_cur = 0;
    caml_young_alloc_start = caml_young_start;
    caml_young_alloc_end = caml_young_semispace_boundary;
  }
  caml_young_alloc_mid =
    caml_young_alloc_start
    + (caml_young_alloc_end - caml_young_alloc_start) / 2;
  caml_young_trigger = caml_young_alloc_mid;
  caml_young_limit = caml_young_trigger;
  caml_young_ptr = caml_young_alloc_end;
  clear_table ((struct generic_table *) &caml_ephe_ref_table,
               (char *) keep_re);
  clear_table ((struct generic_table *) &caml_custom_table,
               (char *) keep_elt);
  caml_extra_heap_resources_minor = 0;
  caml_gc_message (0x02, ">");
  caml_in_minor_collection = 0;
  CAML_INSTR_TIME (tmr, "minor/finalized");
  caml_stat_promoted_words += caml_allocated_words - prev_alloc_words;
  CAML_INSTR_INT ("minor/promoted#", caml_allocated_words - prev_alloc_words);
  ++ caml_stat_minor_collections;
  if (caml_minor_gc_end_hook != NULL) (*caml_minor_gc_end_hook) ();
#ifdef DEBUG
  {
    value *p;
    for (p = caml_young_alloc_start; p < caml_young_alloc_end; ++p){
      *p = Debug_free_minor;
    }
  }
#endif
}

#ifdef CAML_INSTR
extern uintnat caml_instr_alloc_jump;
#endif

/* Do a minor collection or a slice of major collection, call finalisation
   functions, etc.
   Leave enough room in the minor heap to allocate at least one object.
*/
CAMLexport void caml_gc_dispatch (void)
{
#ifdef CAML_INSTR
  CAML_INSTR_SETUP(tmr, "dispatch");
  CAML_INSTR_TIME (tmr, "overhead");
  CAML_INSTR_INT ("alloc/jump#", caml_instr_alloc_jump);
  caml_instr_alloc_jump = 0;
#endif

  if (caml_young_trigger == caml_young_alloc_start){
    /* The minor heap is full, we must do a minor collection. */
    caml_requested_minor_gc = 1;
  }else{
    /* The minor heap is half-full, do a major GC slice. */
    caml_requested_major_slice = 1;
  }
  if (caml_requested_minor_gc){
    caml_requested_minor_gc = 0;
    if (caml_gc_phase == Phase_idle){
      /* Empty the minor heap so we can start a major collection. */
      caml_empty_minor_heap (0.);
      caml_major_collection_slice (-1);
    }else{
      caml_empty_minor_heap (caml_young_aging_ratio);
    }
    CAML_INSTR_TIME (tmr, "dispatch/minor");

    caml_final_do_calls ();
    CAML_INSTR_TIME (tmr, "dispatch/finalizers");

    while (caml_young_ptr - caml_young_alloc_start < Max_young_whsize){
      /* The finalizers or the hooks have filled up the minor heap, we must
         repeat the minor collection. */
      caml_requested_minor_gc = 0;
      caml_empty_minor_heap (caml_young_aging_ratio);
      CAML_INSTR_TIME (tmr, "dispatch/finalizers_minor");
    }
  }
  if (caml_requested_major_slice){
    caml_requested_major_slice = 0;
    caml_young_trigger = caml_young_alloc_start;
    caml_young_limit = caml_young_trigger;
    caml_major_collection_slice (-1);
    CAML_INSTR_TIME (tmr, "dispatch/major");
  }
}

/* For backward compatibility with Lablgtk: do a minor collection to
   ensure that the minor heap is empty.
*/
CAMLexport void caml_minor_collection (void)
{
  caml_empty_minor_heap (0.);
}

CAMLexport value caml_check_urgent_gc (value extra_root)
{
  CAMLparam1 (extra_root);
  if (caml_requested_major_slice || caml_requested_minor_gc){
    CAML_INSTR_INT ("force_minor/check_urgent_gc@", 1);
    caml_gc_dispatch();
  }
  CAMLreturn (extra_root);
}

static void realloc_generic_table
(struct generic_table *tbl, asize_t element_size,
 char * msg_intr_int, char *msg_threshold, char *msg_growing, char *msg_error)
{
  CAMLassert (tbl->ptr == tbl->limit);
  CAMLassert (tbl->limit <= tbl->end);
  CAMLassert (tbl->limit >= tbl->threshold);

  if (tbl->base == NULL){
    alloc_generic_table (tbl, caml_minor_heap_wsz / 8, 256,
                         element_size);
  }else if (tbl->limit == tbl->threshold){
    CAML_INSTR_INT (msg_intr_int, 1);
    caml_gc_message (0x08, msg_threshold, 0);
    tbl->limit = tbl->end;
    caml_request_minor_gc ();
  }else{
    asize_t sz;
    asize_t cur_ptr = tbl->ptr - tbl->base;
    CAMLassert (caml_requested_minor_gc);

    tbl->size *= 2;
    sz = (tbl->size + tbl->reserve) * element_size;
    caml_gc_message (0x08, msg_growing, (intnat) sz/1024);
    tbl->base = caml_stat_resize_noexc (tbl->base, sz);
    if (tbl->base == NULL){
      caml_fatal_error ("%s", msg_error);
    }
    tbl->end = tbl->base + (tbl->size + tbl->reserve) * element_size;
    tbl->threshold = tbl->base + tbl->size * element_size;
    tbl->ptr = tbl->base + cur_ptr;
    tbl->limit = tbl->end;
  }
}

void caml_realloc_ref_table (struct caml_ref_table *tbl)
{
  realloc_generic_table
    ((struct generic_table *) tbl, sizeof (value *),
     "request_minor/realloc_ref_table@",
     "ref_table threshold crossed\n",
     "Growing ref_table to %" ARCH_INTNAT_PRINTF_FORMAT "dk bytes\n",
     "ref_table overflow");
}

void caml_realloc_ephe_ref_table (struct caml_ephe_ref_table *tbl)
{
  realloc_generic_table
    ((struct generic_table *) tbl, sizeof (struct caml_ephe_ref_elt),
     "request_minor/realloc_ephe_ref_table@",
     "ephe_ref_table threshold crossed\n",
     "Growing ephe_ref_table to %" ARCH_INTNAT_PRINTF_FORMAT "dk bytes\n",
     "ephe_ref_table overflow");
}

void caml_realloc_custom_table (struct caml_custom_table *tbl)
{
  realloc_generic_table
    ((struct generic_table *) tbl, sizeof (struct caml_custom_elt),
     "request_minor/realloc_custom_table@",
     "custom_table threshold crossed\n",
     "Growing custom_table to %" ARCH_INTNAT_PRINTF_FORMAT "dk bytes\n",
     "custom_table overflow");
}
