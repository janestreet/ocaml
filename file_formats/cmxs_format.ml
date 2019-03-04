(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2010 Institut National de Recherche en Informatique et     *)
(*     en Automatique                                                     *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Dynunit_info = struct
  type t = {
    unit : Compilation_unit.t;
    crc : Digest.t;
    imports_cmi : Digest.t option Compilation_unit.Map.t;
    imports_cmx : Digest.t option Compilation_unit.Map.t;
    defines : Compilation_unit.t list;
  }

  let create ~unit ~crc ~imports_cmi ~imports_cmx ~defines =
    { unit;
      crc;
      imports_cmi;
      imports_cmx;
      defines;
    }

  let unit t = t.unit
  let crc t = t.crc
  let imports_cmi t = t.imports_cmi
  let imports_cmx t = t.imports_cmx
  let defines t = t.defines
end

module Dynheader_info = struct
  type t = {
    magic : string;
    units : Dynunit_info.t list;
  }

  let create ~units =
    { magic = Config.cmxs_magic_number;
      units;
    }

  let magic t = t.magic
  let units t = t.units
end
