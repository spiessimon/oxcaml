(* TEST
 flags = "-dlambda -ddebug-uids -extension-universe alpha";
 expect;
*)

module Declarations = struct

  (* vec128 *)
  let[@inline never][@locals never] f_int8x16 (x: int8x16) = x
  let[@inline never][@locals never] f_int8x16_unboxed (x: int8x16#) = x
  let[@inline never][@locals never] f_int16x8 (x: int16x8) = x
  let[@inline never][@locals never] f_int16x8_unboxed (x: int16x8#) = x
  let[@inline never][@locals never] f_int32x4 (x: int32x4) = x
  let[@inline never][@locals never] f_int32x4_unboxed (x: int32x4#) = x
  let[@inline never][@locals never] f_int64x2 (x: int64x2) = x
  let[@inline never][@locals never] f_int64x2_unboxed (x: int64x2#) = x
  let[@inline never][@locals never] f_float32x4 (x: float32x4) = x
  let[@inline never][@locals never] f_float32x4_unboxed (x: float32x4#) = x
  let[@inline never][@locals never] f_float64x2 (x: float64x2) = x
  let[@inline never][@locals never] f_float64x2_unboxed (x: float64x2#) = x

  (* vec256 *)
  let[@inline never][@locals never] f_int8x32 (x: int8x32) = x
  let[@inline never][@locals never] f_int8x32_unboxed (x: int8x32#) = x
  let[@inline never][@locals never] f_int16x16 (x: int16x16) = x
  let[@inline never][@locals never] f_int16x16_unboxed (x: int16x16#) = x
  let[@inline never][@locals never] f_int32x8 (x: int32x8) = x
  let[@inline never][@locals never] f_int32x8_unboxed (x: int32x8#) = x
  let[@inline never][@locals never] f_int64x4 (x: int64x4) = x
  let[@inline never][@locals never] f_int64x4_unboxed (x: int64x4#) = x
  let[@inline never][@locals never] f_float32x8 (x: float32x8) = x
  let[@inline never][@locals never] f_float32x8_unboxed (x: float32x8#) = x
  let[@inline never][@locals never] f_float64x4 (x: float64x4) = x
  let[@inline never][@locals never] f_float64x4_unboxed (x: float64x4#) = x


  (* vec512 *)
  let[@inline never][@locals never] f_int8x64 (x: int8x64) = x
  let[@inline never][@locals never] f_int8x64_unboxed (x: int8x64#) = x
  let[@inline never][@locals never] f_int16x32 (x: int16x32) = x
  let[@inline never][@locals never] f_int16x32_unboxed (x: int16x32#) = x
  let[@inline never][@locals never] f_int32x16 (x: int32x16) = x
  let[@inline never][@locals never] f_int32x16_unboxed (x: int32x16#) = x
  let[@inline never][@locals never] f_int64x8 (x: int64x8) = x
  let[@inline never][@locals never] f_int64x8_unboxed (x: int64x8#) = x
  let[@inline never][@locals never] f_float32x16 (x: float32x16) = x
  let[@inline never][@locals never] f_float32x16_unboxed (x: float32x16#) = x
  let[@inline never][@locals never] f_float64x8 (x: float64x8) = x
  let[@inline never][@locals never] f_float64x8_unboxed (x: float64x8#) = x

end
[%%expect{|
(apply (field_imm 1 (global Toploop!)) "Declarations/392"
  (let
    (f_int8x16/284@{.0} =
       (function {nlocal = 0} x/286@{.1}[vec128] never_inline : vec128 x/286)
     f_int8x16_unboxed/287@{.2} =
       (function {nlocal = 0} x/289@{.3}[unboxed_vec128] never_inline
         : unboxed_vec128 x/289)
     f_int16x8/290@{.4} =
       (function {nlocal = 0} x/292@{.5}[vec128] never_inline : vec128 x/292)
     f_int16x8_unboxed/293@{.6} =
       (function {nlocal = 0} x/295@{.7}[unboxed_vec128] never_inline
         : unboxed_vec128 x/295)
     f_int32x4/296@{.8} =
       (function {nlocal = 0} x/298@{.9}[vec128] never_inline : vec128 x/298)
     f_int32x4_unboxed/299@{.10} =
       (function {nlocal = 0} x/301@{.11}[unboxed_vec128] never_inline
         : unboxed_vec128 x/301)
     f_int64x2/302@{.12} =
       (function {nlocal = 0} x/304@{.13}[vec128] never_inline : vec128
         x/304)
     f_int64x2_unboxed/305@{.14} =
       (function {nlocal = 0} x/307@{.15}[unboxed_vec128] never_inline
         : unboxed_vec128 x/307)
     f_float32x4/308@{.16} =
       (function {nlocal = 0} x/310@{.17}[vec128] never_inline : vec128
         x/310)
     f_float32x4_unboxed/311@{.18} =
       (function {nlocal = 0} x/313@{.19}[unboxed_vec128] never_inline
         : unboxed_vec128 x/313)
     f_float64x2/314@{.20} =
       (function {nlocal = 0} x/316@{.21}[vec128] never_inline : vec128
         x/316)
     f_float64x2_unboxed/317@{.22} =
       (function {nlocal = 0} x/319@{.23}[unboxed_vec128] never_inline
         : unboxed_vec128 x/319)
     f_int8x32/320@{.24} =
       (function {nlocal = 0} x/322@{.25}[vec256] never_inline : vec256
         x/322)
     f_int8x32_unboxed/323@{.26} =
       (function {nlocal = 0} x/325@{.27}[unboxed_vec256] never_inline
         : unboxed_vec256 x/325)
     f_int16x16/326@{.28} =
       (function {nlocal = 0} x/328@{.29}[vec256] never_inline : vec256
         x/328)
     f_int16x16_unboxed/329@{.30} =
       (function {nlocal = 0} x/331@{.31}[unboxed_vec256] never_inline
         : unboxed_vec256 x/331)
     f_int32x8/332@{.32} =
       (function {nlocal = 0} x/334@{.33}[vec256] never_inline : vec256
         x/334)
     f_int32x8_unboxed/335@{.34} =
       (function {nlocal = 0} x/337@{.35}[unboxed_vec256] never_inline
         : unboxed_vec256 x/337)
     f_int64x4/338@{.36} =
       (function {nlocal = 0} x/340@{.37}[vec256] never_inline : vec256
         x/340)
     f_int64x4_unboxed/341@{.38} =
       (function {nlocal = 0} x/343@{.39}[unboxed_vec256] never_inline
         : unboxed_vec256 x/343)
     f_float32x8/344@{.40} =
       (function {nlocal = 0} x/346@{.41}[vec256] never_inline : vec256
         x/346)
     f_float32x8_unboxed/347@{.42} =
       (function {nlocal = 0} x/349@{.43}[unboxed_vec256] never_inline
         : unboxed_vec256 x/349)
     f_float64x4/350@{.44} =
       (function {nlocal = 0} x/352@{.45}[vec256] never_inline : vec256
         x/352)
     f_float64x4_unboxed/353@{.46} =
       (function {nlocal = 0} x/355@{.47}[unboxed_vec256] never_inline
         : unboxed_vec256 x/355)
     f_int8x64/356@{.48} =
       (function {nlocal = 0} x/358@{.49}[vec512] never_inline : vec512
         x/358)
     f_int8x64_unboxed/359@{.50} =
       (function {nlocal = 0} x/361@{.51}[unboxed_vec512] never_inline
         : unboxed_vec512 x/361)
     f_int16x32/362@{.52} =
       (function {nlocal = 0} x/364@{.53}[vec512] never_inline : vec512
         x/364)
     f_int16x32_unboxed/365@{.54} =
       (function {nlocal = 0} x/367@{.55}[unboxed_vec512] never_inline
         : unboxed_vec512 x/367)
     f_int32x16/368@{.56} =
       (function {nlocal = 0} x/370@{.57}[vec512] never_inline : vec512
         x/370)
     f_int32x16_unboxed/371@{.58} =
       (function {nlocal = 0} x/373@{.59}[unboxed_vec512] never_inline
         : unboxed_vec512 x/373)
     f_int64x8/374@{.60} =
       (function {nlocal = 0} x/376@{.61}[vec512] never_inline : vec512
         x/376)
     f_int64x8_unboxed/377@{.62} =
       (function {nlocal = 0} x/379@{.63}[unboxed_vec512] never_inline
         : unboxed_vec512 x/379)
     f_float32x16/380@{.64} =
       (function {nlocal = 0} x/382@{.65}[vec512] never_inline : vec512
         x/382)
     f_float32x16_unboxed/383@{.66} =
       (function {nlocal = 0} x/385@{.67}[unboxed_vec512] never_inline
         : unboxed_vec512 x/385)
     f_float64x8/386@{.68} =
       (function {nlocal = 0} x/388@{.69}[vec512] never_inline : vec512
         x/388)
     f_float64x8_unboxed/389@{.70} =
       (function {nlocal = 0} x/391@{.71}[unboxed_vec512] never_inline
         : unboxed_vec512 x/391))
    (makeblock 0 f_int8x16/284 f_int8x16_unboxed/287 f_int16x8/290
      f_int16x8_unboxed/293 f_int32x4/296 f_int32x4_unboxed/299 f_int64x2/302
      f_int64x2_unboxed/305 f_float32x4/308 f_float32x4_unboxed/311
      f_float64x2/314 f_float64x2_unboxed/317 f_int8x32/320
      f_int8x32_unboxed/323 f_int16x16/326 f_int16x16_unboxed/329
      f_int32x8/332 f_int32x8_unboxed/335 f_int64x4/338 f_int64x4_unboxed/341
      f_float32x8/344 f_float32x8_unboxed/347 f_float64x4/350
      f_float64x4_unboxed/353 f_int8x64/356 f_int8x64_unboxed/359
      f_int16x32/362 f_int16x32_unboxed/365 f_int32x16/368
      f_int32x16_unboxed/371 f_int64x8/374 f_int64x8_unboxed/377
      f_float32x16/380 f_float32x16_unboxed/383 f_float64x8/386
      f_float64x8_unboxed/389)))

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| UID                            | Type Declaration                                                                                                                                                              |
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
| UID | Type                                                          | Sort   |
--------------------------------------------------------------------------------
| .0  | Ts_arrow (Ts_predef int8x16 (), Ts_predef int8x16 ())         | value  |
| .1  | Ts_predef int8x16 ()                                          | value  |
| .2  | Ts_arrow (Ts_predef int8x16# (), Ts_predef int8x16# ())       | value  |
| .3  | Ts_predef int8x16# ()                                         | vec128 |
| .4  | Ts_arrow (Ts_predef int16x8 (), Ts_predef int16x8 ())         | value  |
| .5  | Ts_predef int16x8 ()                                          | value  |
| .6  | Ts_arrow (Ts_predef int16x8# (), Ts_predef int16x8# ())       | value  |
| .7  | Ts_predef int16x8# ()                                         | vec128 |
| .8  | Ts_arrow (Ts_predef int32x4 (), Ts_predef int32x4 ())         | value  |
| .9  | Ts_predef int32x4 ()                                          | value  |
| .10 | Ts_arrow (Ts_predef int32x4# (), Ts_predef int32x4# ())       | value  |
| .11 | Ts_predef int32x4# ()                                         | vec128 |
| .12 | Ts_arrow (Ts_predef int64x2 (), Ts_predef int64x2 ())         | value  |
| .13 | Ts_predef int64x2 ()                                          | value  |
| .14 | Ts_arrow (Ts_predef int64x2# (), Ts_predef int64x2# ())       | value  |
| .15 | Ts_predef int64x2# ()                                         | vec128 |
| .16 | Ts_arrow (Ts_predef float32x4 (), Ts_predef float32x4 ())     | value  |
| .17 | Ts_predef float32x4 ()                                        | value  |
| .18 | Ts_arrow (Ts_predef float32x4# (), Ts_predef float32x4# ())   | value  |
| .19 | Ts_predef float32x4# ()                                       | vec128 |
| .20 | Ts_arrow (Ts_predef float64x2 (), Ts_predef float64x2 ())     | value  |
| .21 | Ts_predef float64x2 ()                                        | value  |
| .22 | Ts_arrow (Ts_predef float64x2# (), Ts_predef float64x2# ())   | value  |
| .23 | Ts_predef float64x2# ()                                       | vec128 |
| .24 | Ts_arrow (Ts_predef int8x32 (), Ts_predef int8x32 ())         | value  |
| .25 | Ts_predef int8x32 ()                                          | value  |
| .26 | Ts_arrow (Ts_predef int8x32# (), Ts_predef int8x32# ())       | value  |
| .27 | Ts_predef int8x32# ()                                         | vec256 |
| .28 | Ts_arrow (Ts_predef int16x16 (), Ts_predef int16x16 ())       | value  |
| .29 | Ts_predef int16x16 ()                                         | value  |
| .30 | Ts_arrow (Ts_predef int16x16# (), Ts_predef int16x16# ())     | value  |
| .31 | Ts_predef int16x16# ()                                        | vec256 |
| .32 | Ts_arrow (Ts_predef int32x8 (), Ts_predef int32x8 ())         | value  |
| .33 | Ts_predef int32x8 ()                                          | value  |
| .34 | Ts_arrow (Ts_predef int32x8# (), Ts_predef int32x8# ())       | value  |
| .35 | Ts_predef int32x8# ()                                         | vec256 |
| .36 | Ts_arrow (Ts_predef int64x4 (), Ts_predef int64x4 ())         | value  |
| .37 | Ts_predef int64x4 ()                                          | value  |
| .38 | Ts_arrow (Ts_predef int64x4# (), Ts_predef int64x4# ())       | value  |
| .39 | Ts_predef int64x4# ()                                         | vec256 |
| .40 | Ts_arrow (Ts_predef float32x8 (), Ts_predef float32x8 ())     | value  |
| .41 | Ts_predef float32x8 ()                                        | value  |
| .42 | Ts_arrow (Ts_predef float32x8# (), Ts_predef float32x8# ())   | value  |
| .43 | Ts_predef float32x8# ()                                       | vec256 |
| .44 | Ts_arrow (Ts_predef float64x4 (), Ts_predef float64x4 ())     | value  |
| .45 | Ts_predef float64x4 ()                                        | value  |
| .46 | Ts_arrow (Ts_predef float64x4# (), Ts_predef float64x4# ())   | value  |
| .47 | Ts_predef float64x4# ()                                       | vec256 |
| .48 | Ts_arrow (Ts_predef int8x64 (), Ts_predef int8x64 ())         | value  |
| .49 | Ts_predef int8x64 ()                                          | value  |
| .50 | Ts_arrow (Ts_predef int8x64# (), Ts_predef int8x64# ())       | value  |
| .51 | Ts_predef int8x64# ()                                         | vec512 |
| .52 | Ts_arrow (Ts_predef int16x32 (), Ts_predef int16x32 ())       | value  |
| .53 | Ts_predef int16x32 ()                                         | value  |
| .54 | Ts_arrow (Ts_predef int16x32# (), Ts_predef int16x32# ())     | value  |
| .55 | Ts_predef int16x32# ()                                        | vec512 |
| .56 | Ts_arrow (Ts_predef int32x16 (), Ts_predef int32x16 ())       | value  |
| .57 | Ts_predef int32x16 ()                                         | value  |
| .58 | Ts_arrow (Ts_predef int32x16# (), Ts_predef int32x16# ())     | value  |
| .59 | Ts_predef int32x16# ()                                        | vec512 |
| .60 | Ts_arrow (Ts_predef int64x8 (), Ts_predef int64x8 ())         | value  |
| .61 | Ts_predef int64x8 ()                                          | value  |
| .62 | Ts_arrow (Ts_predef int64x8# (), Ts_predef int64x8# ())       | value  |
| .63 | Ts_predef int64x8# ()                                         | vec512 |
| .64 | Ts_arrow (Ts_predef float32x16 (), Ts_predef float32x16 ())   | value  |
| .65 | Ts_predef float32x16 ()                                       | value  |
| .66 | Ts_arrow (Ts_predef float32x16# (), Ts_predef float32x16# ()) | value  |
| .67 | Ts_predef float32x16# ()                                      | vec512 |
| .68 | Ts_arrow (Ts_predef float64x8 (), Ts_predef float64x8 ())     | value  |
| .69 | Ts_predef float64x8 ()                                        | value  |
| .70 | Ts_arrow (Ts_predef float64x8# (), Ts_predef float64x8# ())   | value  |
| .71 | Ts_predef float64x8# ()                                       | vec512 |
--------------------------------------------------------------------------------

module Declarations :
  sig
    val f_int8x16 : int8x16 -> int8x16
    val f_int8x16_unboxed : int8x16# -> int8x16#
    val f_int16x8 : int16x8 -> int16x8
    val f_int16x8_unboxed : int16x8# -> int16x8#
    val f_int32x4 : int32x4 -> int32x4
    val f_int32x4_unboxed : int32x4# -> int32x4#
    val f_int64x2 : int64x2 -> int64x2
    val f_int64x2_unboxed : int64x2# -> int64x2#
    val f_float32x4 : float32x4 -> float32x4
    val f_float32x4_unboxed : float32x4# -> float32x4#
    val f_float64x2 : float64x2 -> float64x2
    val f_float64x2_unboxed : float64x2# -> float64x2#
    val f_int8x32 : int8x32 -> int8x32
    val f_int8x32_unboxed : int8x32# -> int8x32#
    val f_int16x16 : int16x16 -> int16x16
    val f_int16x16_unboxed : int16x16# -> int16x16#
    val f_int32x8 : int32x8 -> int32x8
    val f_int32x8_unboxed : int32x8# -> int32x8#
    val f_int64x4 : int64x4 -> int64x4
    val f_int64x4_unboxed : int64x4# -> int64x4#
    val f_float32x8 : float32x8 -> float32x8
    val f_float32x8_unboxed : float32x8# -> float32x8#
    val f_float64x4 : float64x4 -> float64x4
    val f_float64x4_unboxed : float64x4# -> float64x4#
    val f_int8x64 : int8x64 -> int8x64
    val f_int8x64_unboxed : int8x64# -> int8x64#
    val f_int16x32 : int16x32 -> int16x32
    val f_int16x32_unboxed : int16x32# -> int16x32#
    val f_int32x16 : int32x16 -> int32x16
    val f_int32x16_unboxed : int32x16# -> int32x16#
    val f_int64x8 : int64x8 -> int64x8
    val f_int64x8_unboxed : int64x8# -> int64x8#
    val f_float32x16 : float32x16 -> float32x16
    val f_float32x16_unboxed : float32x16# -> float32x16#
    val f_float64x8 : float64x8 -> float64x8
    val f_float64x8_unboxed : float64x8# -> float64x8#
  end
|}]
