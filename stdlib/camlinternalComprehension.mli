
val map: ('a -> 'b)  -> 'a list ->'b list
val map_from_to: (int -> 'a)  -> int -> int -> 'a list
val map_from_downto: (int -> 'a) -> int -> int  -> 'a list


val array_map: ('a -> 'b)  -> 'a array ->'b array
val array_map_from_to: (int -> 'a)  -> int -> int -> 'a array
val array_map_from_downto: (int -> 'a) -> int -> int  -> 'a array

val concat_map: ('a -> 'b list) -> 'a list -> 'b list 
val concat_map_from_to: (int -> 'a list) -> int -> int -> 'a list
val concat_map_from_downto: (int -> 'a list) -> int -> int -> 'a list

