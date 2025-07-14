(* TEST
 flags = "-dlambda -ddebug-uids -extension-universe beta";
 expect;
*)

module Declarations = struct
  let[@inline never][@locals never] f_int (x: int) = x
  let[@inline never][@locals never] f_char (x: char) = x
  let[@inline never][@locals never] f_string (x: string) = x
  let[@inline never][@locals never] f_bytes (x: bytes) = x
  let[@inline never][@locals never] f_float (x: float) = x
  let[@inline never][@locals never] f_float32 (x: float32) = x
  let[@inline never][@locals never] f_bool (x: bool) = x
  let[@inline never][@locals never] f_unit (x: unit) = x
  let[@inline never][@locals never] f_exn (x: exn) = x
  let[@inline never][@locals never] f_array (x: char array) = x
  let[@inline never][@locals never] f_iarray (x: char iarray) = x
  let[@inline never][@locals never] f_list (x: char list) = x
  let[@inline never][@locals never] f_array (x: char option) = x
  let[@inline never][@locals never] f_nativeint (x: nativeint) = x
  let[@inline never][@locals never] f_int8 (x: int8) = x
  let[@inline never][@locals never] f_int16 (x: int16) = x
  let[@inline never][@locals never] f_int32 (x: int32) = x
  let[@inline never][@locals never] f_int64 (x: int64) = x
  let[@inline never][@locals never] f_lazy (x: char lazy_t) = x


  (* BASIC UNBOXED TYPES *)

  type b = #(int64# * bool)
  let[@inline never][@locals never] f_unboxed_float (x: float#) = x
  let[@inline never][@locals never] f_unboxed_float32 (z: float32#) = z
  let[@inline never][@locals never] f_unboxed_nativeint (x: nativeint#) = x
  let[@inline never][@locals never] f_unboxed_int32 (x: int32#) = x
  let[@inline never][@locals never] f_unboxed_int64 (x: int64#) = x
  let[@inline never][@locals never] f_product (x: #(int64# * bool)) = x
  let[@inline never][@locals never] f_defined_product (x: b) = x
  let[@inline never][@locals never] f_nested_product (x: #(b * b)) = x
  let[@inline never][@locals never] f_bits64 (type (a : bits64)) (x: a) = x
  let[@inline never][@locals never] f_float64 (type (a : float64)) (x: a) = x

end
[%%expect{|
(apply (field_imm 1 (global Toploop!)) "Declarations/373"
  (let
    (f_int/284@{.0} =
       (function {nlocal = 0} x/286@{.1}[int] never_inline : int x/286)
     f_char/287@{.2} =
       (function {nlocal = 0} x/289@{.3}[int] never_inline : int x/289)
     f_string/290@{.4} =
       (function {nlocal = 0} x/292@{.5} never_inline x/292)
     f_bytes/293@{.6} = (function {nlocal = 0} x/295@{.7} never_inline x/295)
     f_float/296@{.8} =
       (function {nlocal = 0} x/298@{.9}[float] never_inline : float x/298)
     f_float32/299@{.10} =
       (function {nlocal = 0} x/301@{.11}[float32] never_inline : float32
         x/301)
     f_bool/302@{.12} =
       (function {nlocal = 0} x/304@{.13}[int] never_inline : int x/304)
     f_unit/305@{.14} =
       (function {nlocal = 0} x/307@{.15}[int] never_inline : int x/307)
     f_exn/308@{.16} = (function {nlocal = 0} x/310@{.17} never_inline x/310)
     f_array/311@{.18} =
       (function {nlocal = 0} x/313@{.19}[intarray] never_inline : intarray
         x/313)
     f_iarray/314@{.20} =
       (function {nlocal = 0} x/316@{.21} never_inline x/316)
     f_list/317@{.22} =
       (function {nlocal = 0}
         x/319@{.23}[(consts (0))
                     (non_consts ([0: *,
                                   [(consts (0)) (non_consts ([0: *, *]))]]))]
         never_inline
         [(consts (0))
          (non_consts ([0: *, [(consts (0)) (non_consts ([0: *, *]))]]))]x/319)
     f_array/320@{.24} =
       (function {nlocal = 0} x/321@{.25}[(consts (0)) (non_consts ([0: *]))]
         never_inline [(consts (0)) (non_consts ([0: *]))]x/321)
     f_nativeint/322@{.26} =
       (function {nlocal = 0} x/324@{.27}[nativeint] never_inline : nativeint
         x/324)
     f_int8/325@{.28} =
       (function {nlocal = 0} x/327@{.29}[int] never_inline : int x/327)
     f_int16/328@{.30} =
       (function {nlocal = 0} x/330@{.31}[int] never_inline : int x/330)
     f_int32/331@{.32} =
       (function {nlocal = 0} x/333@{.33}[int32] never_inline : int32 x/333)
     f_int64/334@{.34} =
       (function {nlocal = 0} x/336@{.35}[int64] never_inline : int64 x/336)
     f_lazy/337@{.36} =
       (function {nlocal = 0} x/339@{.37} never_inline x/339)
     f_unboxed_float/341@{.39} =
       (function {nlocal = 0} x/343@{.40}[unboxed_float] never_inline
         : unboxed_float x/343)
     f_unboxed_float32/344@{.41} =
       (function {nlocal = 0} z/346@{.42}[unboxed_float32] never_inline
         : unboxed_float32 z/346)
     f_unboxed_nativeint/347@{.43} =
       (function {nlocal = 0} x/349@{.44}[unboxed_nativeint] never_inline
         : unboxed_nativeint x/349)
     f_unboxed_int32/350@{.45} =
       (function {nlocal = 0} x/352@{.46}[unboxed_int32] never_inline
         : unboxed_int32 x/352)
     f_unboxed_int64/353@{.47} =
       (function {nlocal = 0} x/355@{.48}[unboxed_int64] never_inline
         : unboxed_int64 x/355)
     f_product/356@{.49} =
       (function {nlocal = 0} x/358@{.50}#([unboxed_int64], *) never_inline
         : #([unboxed_int64], *)x/358)
     f_defined_product/359@{.51} =
       (function {nlocal = 0} x/361@{.52}#([unboxed_int64], *) never_inline
         : #([unboxed_int64], *)x/361)
     f_nested_product/362@{.53} =
       (function {nlocal = 0}
         x/364@{.54}#(#([unboxed_int64], *), #([unboxed_int64], *))
         never_inline : #(#([unboxed_int64], *), #([unboxed_int64], *))x/364)
     f_bits64/365@{.55} =
       (function {nlocal = 0} x/368@{.57}[unboxed_int64] never_inline
         : unboxed_int64 x/368)
     f_float64/369@{.58} =
       (function {nlocal = 0} x/372@{.60}[unboxed_float] never_inline
         : unboxed_float x/372))
    (makeblock 0 f_int/284 f_char/287 f_string/290 f_bytes/293 f_float/296
      f_float32/299 f_bool/302 f_unit/305 f_exn/308 f_iarray/314 f_list/317
      f_array/320 f_nativeint/322 f_int8/325 f_int16/328 f_int32/331
      f_int64/334 f_lazy/337 f_unboxed_float/341 f_unboxed_float32/344
      f_unboxed_nativeint/347 f_unboxed_int32/350 f_unboxed_int64/353
      f_product/356 f_defined_product/359 f_nested_product/362 f_bits64/365
      f_float64/369)))

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| UID                            | Type Declaration                                                                                                                                                              |
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| .38                            | path=b/340[2], definition=(Tds_alias Ts_unboxed_tuple (Ts_predef int64# (), Ts_constr uid=<predef:bool> path=bool/6! ()))                                                     |
| <predef:array>                 | path=array/9!, definition=(Tds_other)                                                                                                                                         |
| <predef:bool>                  | path=bool/6!, definition=(Tds_variant simple_constructors=false | true complex_constructors=)                                                                                 |
| <predef:bytes>                 | path=bytes/3!, definition=(Tds_other)                                                                                                                                         |
| <predef:char>                  | path=char/2!, definition=(Tds_other)                                                                                                                                          |
| <predef:exn>                   | path=exn/8!, definition=(Tds_other)                                                                                                                                           |
| <predef:extension_constructor> | path=extension_constructor/20!, definition=(Tds_other)                                                                                                                        |
| <predef:float>                 | path=float/4!, definition=(Tds_other)                                                                                                                                         |
| <predef:floatarray>            | path=floatarray/21!, definition=(Tds_other)                                                                                                                                   |
| <predef:iarray>                | path=iarray/10!, definition=(Tds_other)                                                                                                                                       |
| <predef:int>                   | path=int/1!, definition=(Tds_other)                                                                                                                                           |
| <predef:int32>                 | path=int32/16!, definition=(Tds_other)                                                                                                                                        |
| <predef:int64>                 | path=int64/17!, definition=(Tds_other)                                                                                                                                        |
| <predef:lazy_t>                | path=lazy_t/18!, definition=(Tds_other)                                                                                                                                       |
| <predef:lexing_position>       | path=lexing_position/22!, definition=(Tds_record_boxed { pos_fname: Ts_predef string (); pos_lnum: Ts_predef int (); pos_bol: Ts_predef int (); pos_cnum: Ts_predef int () }) |
| <predef:list>                  | path=list/11!, definition=(Tds_variant simple_constructors=[] complex_constructors=(:: of Ts_var () * Ts_constr uid=<predef:list> path=list/11! (Ts_var ())))                 |
| <predef:nativeint>             | path=nativeint/13!, definition=(Tds_other)                                                                                                                                    |
| <predef:option>                | path=option/12!, definition=(Tds_variant simple_constructors=None complex_constructors=(Some of Ts_var ()))                                                                   |
| <predef:string>                | path=string/19!, definition=(Tds_other)                                                                                                                                       |
| <predef:unit>                  | path=unit/7!, definition=(Tds_variant simple_constructors=() complex_constructors=)                                                                                           |
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| UID | Type                                                                                                                                                                                            | Sort                                |
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| .0  | Ts_arrow (Ts_predef int (), Ts_predef int ())                                                                                                                                                   | value                               |
| .1  | Ts_predef int ()                                                                                                                                                                                | value                               |
| .2  | Ts_arrow (Ts_predef char (), Ts_predef char ())                                                                                                                                                 | value                               |
| .3  | Ts_predef char ()                                                                                                                                                                               | value                               |
| .4  | Ts_arrow (Ts_predef string (), Ts_predef string ())                                                                                                                                             | value                               |
| .5  | Ts_predef string ()                                                                                                                                                                             | value                               |
| .6  | Ts_arrow (Ts_predef bytes (), Ts_predef bytes ())                                                                                                                                               | value                               |
| .7  | Ts_predef bytes ()                                                                                                                                                                              | value                               |
| .8  | Ts_arrow (Ts_predef float (), Ts_predef float ())                                                                                                                                               | value                               |
| .9  | Ts_predef float ()                                                                                                                                                                              | value                               |
| .10 | Ts_arrow (Ts_predef float32 (), Ts_predef float32 ())                                                                                                                                           | value                               |
| .11 | Ts_predef float32 ()                                                                                                                                                                            | value                               |
| .12 | Ts_arrow (Ts_constr uid=<predef:bool> path=bool/6! (), Ts_constr uid=<predef:bool> path=bool/6! ())                                                                                             | value                               |
| .13 | Ts_constr uid=<predef:bool> path=bool/6! ()                                                                                                                                                     | value                               |
| .14 | Ts_arrow (Ts_constr uid=<predef:unit> path=unit/7! (), Ts_constr uid=<predef:unit> path=unit/7! ())                                                                                             | value                               |
| .15 | Ts_constr uid=<predef:unit> path=unit/7! ()                                                                                                                                                     | value                               |
| .16 | Ts_arrow (Ts_predef exn (), Ts_predef exn ())                                                                                                                                                   | value                               |
| .17 | Ts_predef exn ()                                                                                                                                                                                | value                               |
| .18 | Ts_arrow (Ts_predef array (Ts_predef char ()), Ts_predef array (Ts_predef char ()))                                                                                                             | value                               |
| .19 | Ts_predef array (Ts_predef char ())                                                                                                                                                             | value                               |
| .20 | Ts_arrow (Ts_constr uid=<predef:iarray> path=iarray/10! (Ts_predef char ()), Ts_constr uid=<predef:iarray> path=iarray/10! (Ts_predef char ()))                                                 | value                               |
| .21 | Ts_constr uid=<predef:iarray> path=iarray/10! (Ts_predef char ())                                                                                                                               | value                               |
| .22 | Ts_arrow (Ts_constr uid=<predef:list> path=list/11! (Ts_predef char ()), Ts_constr uid=<predef:list> path=list/11! (Ts_predef char ()))                                                         | value                               |
| .23 | Ts_constr uid=<predef:list> path=list/11! (Ts_predef char ())                                                                                                                                   | value                               |
| .24 | Ts_arrow (Ts_constr uid=<predef:option> path=option/12! (Ts_predef char ()), Ts_constr uid=<predef:option> path=option/12! (Ts_predef char ()))                                                 | value                               |
| .25 | Ts_constr uid=<predef:option> path=option/12! (Ts_predef char ())                                                                                                                               | value                               |
| .26 | Ts_arrow (Ts_predef nativeint (), Ts_predef nativeint ())                                                                                                                                       | value                               |
| .27 | Ts_predef nativeint ()                                                                                                                                                                          | value                               |
| .28 | Ts_arrow (Ts_constr uid=<predef:int8> path=int8/14! (), Ts_constr uid=<predef:int8> path=int8/14! ())                                                                                           | value                               |
| .29 | Ts_constr uid=<predef:int8> path=int8/14! ()                                                                                                                                                    | value                               |
| .30 | Ts_arrow (Ts_constr uid=<predef:int16> path=int16/15! (), Ts_constr uid=<predef:int16> path=int16/15! ())                                                                                       | value                               |
| .31 | Ts_constr uid=<predef:int16> path=int16/15! ()                                                                                                                                                  | value                               |
| .32 | Ts_arrow (Ts_predef int32 (), Ts_predef int32 ())                                                                                                                                               | value                               |
| .33 | Ts_predef int32 ()                                                                                                                                                                              | value                               |
| .34 | Ts_arrow (Ts_predef int64 (), Ts_predef int64 ())                                                                                                                                               | value                               |
| .35 | Ts_predef int64 ()                                                                                                                                                                              | value                               |
| .36 | Ts_arrow (Ts_predef lazy_t (Ts_predef char ()), Ts_predef lazy_t (Ts_predef char ()))                                                                                                           | value                               |
| .37 | Ts_predef lazy_t (Ts_predef char ())                                                                                                                                                            | value                               |
| .39 | Ts_arrow (Ts_predef float# (), Ts_predef float# ())                                                                                                                                             | value                               |
| .40 | Ts_predef float# ()                                                                                                                                                                             | float64                             |
| .41 | Ts_arrow (Ts_predef float32# (), Ts_predef float32# ())                                                                                                                                         | value                               |
| .42 | Ts_predef float32# ()                                                                                                                                                                           | float32                             |
| .43 | Ts_arrow (Ts_predef nativeint# (), Ts_predef nativeint# ())                                                                                                                                     | value                               |
| .44 | Ts_predef nativeint# ()                                                                                                                                                                         | word                                |
| .45 | Ts_arrow (Ts_predef int32# (), Ts_predef int32# ())                                                                                                                                             | value                               |
| .46 | Ts_predef int32# ()                                                                                                                                                                             | bits32                              |
| .47 | Ts_arrow (Ts_predef int64# (), Ts_predef int64# ())                                                                                                                                             | value                               |
| .48 | Ts_predef int64# ()                                                                                                                                                                             | bits64                              |
| .49 | Ts_arrow (Ts_unboxed_tuple (Ts_predef int64# (), Ts_constr uid=<predef:bool> path=bool/6! ()), Ts_unboxed_tuple (Ts_predef int64# (), Ts_constr uid=<predef:bool> path=bool/6! ()))             | value                               |
| .50 | Ts_unboxed_tuple (Ts_predef int64# (), Ts_constr uid=<predef:bool> path=bool/6! ())                                                                                                             | bits64 & value                      |
| .51 | Ts_arrow (Ts_constr uid=.38 path=b/340[2] (), Ts_constr uid=.38 path=b/340[2] ())                                                                                                               | value                               |
| .52 | Ts_constr uid=.38 path=b/340[2] ()                                                                                                                                                              | bits64 & value                      |
| .53 | Ts_arrow (Ts_unboxed_tuple (Ts_constr uid=.38 path=b/340[2] (), Ts_constr uid=.38 path=b/340[2] ()), Ts_unboxed_tuple (Ts_constr uid=.38 path=b/340[2] (), Ts_constr uid=.38 path=b/340[2] ())) | value                               |
| .54 | Ts_unboxed_tuple (Ts_constr uid=.38 path=b/340[2] (), Ts_constr uid=.38 path=b/340[2] ())                                                                                                       | (bits64 & value) & (bits64 & value) |
| .55 | Ts_arrow (Ts_var (a), Ts_var (a))                                                                                                                                                               | value                               |
| .57 | Ts_constr uid=.56 path=a/367[5] ()                                                                                                                                                              | bits64                              |
| .58 | Ts_arrow (Ts_var (a), Ts_var (a))                                                                                                                                                               | value                               |
| .60 | Ts_constr uid=.59 path=a/371[5] ()                                                                                                                                                              | float64                             |
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

module Declarations :
  sig
    val f_int : int -> int
    val f_char : char -> char
    val f_string : string -> string
    val f_bytes : bytes -> bytes
    val f_float : float -> float
    val f_float32 : float32 -> float32
    val f_bool : bool -> bool
    val f_unit : unit -> unit
    val f_exn : exn -> exn
    val f_iarray : char iarray -> char iarray
    val f_list : char list -> char list
    val f_array : char option -> char option
    val f_nativeint : nativeint -> nativeint
    val f_int8 : int8 -> int8
    val f_int16 : int16 -> int16
    val f_int32 : int32 -> int32
    val f_int64 : int64 -> int64
    val f_lazy : char lazy_t -> char lazy_t
    type b = #(int64# * bool)
    val f_unboxed_float : float# -> float#
    val f_unboxed_float32 : float32# -> float32#
    val f_unboxed_nativeint : nativeint# -> nativeint#
    val f_unboxed_int32 : int32# -> int32#
    val f_unboxed_int64 : int64# -> int64#
    val f_product : #(int64# * bool) -> #(int64# * bool)
    val f_defined_product : b -> b
    val f_nested_product : #(b * b) -> #(b * b)
    val f_bits64 : ('a : bits64). 'a -> 'a
    val f_float64 : ('a : float64). 'a -> 'a
  end
|}]
