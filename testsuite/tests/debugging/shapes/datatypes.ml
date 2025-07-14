(* TEST
 flags = "-dlambda -ddebug-uids -extension-universe alpha";
 expect;
*)

type t_void : void

module Declarations = struct

  type enum = A | B | C
  type proper_variant = D of float | E of int | F | H of { x: int; y: float } | G of { x : int; y: int32#; y2: int32#;  z : bool; a : int32# } | I of { x : float; y: float }
  type record_basic = { x: int; y: int }
  type record_float = { x: float; y: float }
  type record_unboxed_tuple = #{ x: float#; y: int32# }
  type record_unboxed = { x: int }[@@unboxed]
  type record_unboxed_unboxed = { x: record_unboxed_tuple }[@@unboxed]
  type record_mixed = { x: float; y: int32#; y2: int32#; z: int }
  type record_unboxed_nested = { foo: record_unboxed_tuple; bar : int }

  type unboxed_variant_record = Bar of {x : float# } [@@unboxed]
  type unboxed_variant_value = Baz of record_float [@@unboxed]
  type unboxed_variant_unboxed_float = Baz of float# [@@unboxed]

  type inner_record = { x: int; y: float }
  and outer_record = { t: inner_record; z: bool }


  type is_void = t_void
  type tuple_with_void = #(t_void * int)
  type unboxed_record_with_void = #{ x : int; y : t_void }


  let[@inline never][@locals never] f_enum (x: enum) = x
  let[@inline never][@locals never] f_proper_variant (x: proper_variant) = x
  let[@inline never][@locals never] f_record_basic (x: record_basic) = x
  let[@inline never][@locals never] f_record_float (x: record_float) = x
  let[@inline never][@locals never] f_record_unboxed (x: record_unboxed) = x
  let[@inline never][@locals never] f_record_unboxed_unboxed (x: record_unboxed_unboxed) = x
  let[@inline never][@locals never] f_record_mixed (x: record_mixed) = x
  let[@inline never][@locals never] f_record_unboxed_nested (x: record_unboxed_nested) = x
  let[@inline never][@locals never] f_unboxed_variant_record (x: unboxed_variant_record) = x
  let[@inline never][@locals never] f_unboxed_variant_value (x: unboxed_variant_value) = x
  let[@inline never][@locals never] f_unboxed_variant_unboxed_float (x: unboxed_variant_unboxed_float) = x
  let[@inline never][@locals never] f_inner_record (x: inner_record) = x
  let[@inline never][@locals never] f_outer_record (x: outer_record) = x

  let[@inline never][@locals never] f_is_void (x: is_void) = x
  let[@inline never][@locals never] f_tuple_with_void (x: tuple_with_void) = x
  let[@inline never][@locals never] f_unboxed_record_with_void (x: unboxed_record_with_void) = x


end
[%%expect{|
0

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| UID                            | Type Declaration                                                                                                                                                              |
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| .0                             | path=t_void/284[1], definition=(Tds_other)                                                                                                                                    |
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

---------------------
| UID | Type | Sort |
---------------------
---------------------

type t_void : void
(apply (field_imm 1 (global Toploop!)) "Declarations/406"
  (let
    (f_enum/358@{.84} =
       (function {nlocal = 0} x/360@{.85}[int] never_inline : int x/360)
     f_proper_variant/361@{.86} =
       (function {nlocal = 0}
         x/363@{.87}[(consts (0)) (non_consts ([4: [float], [float]]
                     [3: [int], bits32, bits32, [int], bits32]
                     [2: [int], [float]] [1: [int]] [0: [float]]))]
         never_inline
         [(consts (0)) (non_consts ([4: [float], [float]]
          [3: [int], bits32, bits32, [int], bits32] [2: [int], [float]]
          [1: [int]] [0: [float]]))]x/363)
     f_record_basic/364@{.88} =
       (function {nlocal = 0}
         x/366@{.89}[(consts ()) (non_consts ([0: [int], [int]]))]
         never_inline [(consts ()) (non_consts ([0: [int], [int]]))]x/366)
     f_record_float/367@{.90} =
       (function {nlocal = 0}
         x/369@{.91}[(consts ()) (non_consts ([254: [float], [float]]))]
         never_inline
         [(consts ()) (non_consts ([254: [float], [float]]))]x/369)
     f_record_unboxed/370@{.92} =
       (function {nlocal = 0} x/372@{.93}[int] never_inline : int x/372)
     f_record_unboxed_unboxed/373@{.94} =
       (function {nlocal = 0} x/375@{.95}#([unboxed_float], [unboxed_int32])
         never_inline : #([unboxed_float], [unboxed_int32])x/375)
     f_record_mixed/376@{.96} =
       (function {nlocal = 0}
         x/378@{.97}[(consts ())
                     (non_consts ([0: [float], bits32, bits32, [int]]))]
         never_inline
         [(consts ()) (non_consts ([0: [float], bits32, bits32, [int]]))]x/378)
     f_record_unboxed_nested/379@{.98} =
       (function {nlocal = 0}
         x/381@{.99}[(consts ())
                     (non_consts ([0: product float64, bits32, [int]]))]
         never_inline
         [(consts ()) (non_consts ([0: product float64, bits32, [int]]))]x/381)
     f_unboxed_variant_record/382@{.100} =
       (function {nlocal = 0} x/384@{.101}[unboxed_float] never_inline
         : unboxed_float x/384)
     f_unboxed_variant_value/385@{.102} =
       (function {nlocal = 0}
         x/387@{.103}[(consts ()) (non_consts ([254: [float], [float]]))]
         never_inline
         [(consts ()) (non_consts ([254: [float], [float]]))]x/387)
     f_unboxed_variant_unboxed_float/388@{.104} =
       (function {nlocal = 0} x/390@{.105}[unboxed_float] never_inline
         : unboxed_float x/390)
     f_inner_record/391@{.106} =
       (function {nlocal = 0}
         x/393@{.107}[(consts ()) (non_consts ([0: [int], [float]]))]
         never_inline [(consts ()) (non_consts ([0: [int], [float]]))]x/393)
     f_outer_record/394@{.108} =
       (function {nlocal = 0}
         x/396@{.109}[(consts ())
                      (non_consts ([0:
                                    [(consts ())
                                     (non_consts ([0: [int], [float]]))],
                                    [int]]))]
         never_inline
         [(consts ())
          (non_consts ([0: [(consts ()) (non_consts ([0: [int], [float]]))],
                        [int]]))]x/396)
     f_is_void/397@{.110} =
       (function {nlocal = 0} x/399@{.111}#() never_inline : #()x/399)
     f_tuple_with_void/400@{.112} =
       (function {nlocal = 0} x/402@{.113}#(#(), *) never_inline
         : #(#(), *)x/402)
     f_unboxed_record_with_void/403@{.114} =
       (function {nlocal = 0} x/405@{.115}#(*, #()) never_inline
         : #(*, #())x/405))
    (makeblock 0 f_enum/358 f_proper_variant/361 f_record_basic/364
      f_record_float/367 f_record_unboxed/370 f_record_unboxed_unboxed/373
      f_record_mixed/376 f_record_unboxed_nested/379
      f_unboxed_variant_record/382 f_unboxed_variant_value/385
      f_unboxed_variant_unboxed_float/388 f_inner_record/391
      f_outer_record/394 f_is_void/397 f_tuple_with_void/400
      f_unboxed_record_with_void/403)))

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| UID                            | Type Declaration                                                                                                                                                                                                                                                                                                                                                                                                 |
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| .0                             | path=t_void/284[1], definition=(Tds_other)                                                                                                                                                                                                                                                                                                                                                                       |
| .1                             | path=enum/285[3], definition=(Tds_variant simple_constructors=A | B | C complex_constructors=)                                                                                                                                                                                                                                                                                                                   |
| .5                             | path=proper_variant/289[4], definition=(Tds_variant simple_constructors=F complex_constructors=(D of Ts_predef float ()) | (E of Ts_predef int ()) | (H of x=Ts_predef int () * y=Ts_predef float ()) | (G of x=Ts_predef int () * y=Ts_predef int32# () * y2=Ts_predef int32# () * z=Ts_constr uid=<predef:bool> path=bool/6! () * a=Ts_predef int32# ()) | (I of x=Ts_predef float () * y=Ts_predef float ())) |
| .39                            | path=record_basic/305[5], definition=(Tds_record_boxed { x: Ts_predef int (); y: Ts_predef int () })                                                                                                                                                                                                                                                                                                             |
| .42                            | path=record_float/310[6], definition=(Tds_record_floats { x: Ts_predef float# (); y: Ts_predef float# () })                                                                                                                                                                                                                                                                                                      |
| .45                            | path=record_unboxed_tuple/315[7], definition=(Tds_record_unboxed_product { x: Ts_predef float# (); y: Ts_predef int32# () })                                                                                                                                                                                                                                                                                     |
| .48                            | path=record_unboxed/318[8], definition=(Tds_record [@@unboxed] { x: Ts_predef int () })                                                                                                                                                                                                                                                                                                                          |
| .50                            | path=record_unboxed_unboxed/320[9], definition=(Tds_record [@@unboxed] { x: Ts_constr uid=.45 path=record_unboxed_tuple/315[7] () })                                                                                                                                                                                                                                                                             |
| .52                            | path=record_mixed/322[10], definition=(Tds_record_mixed { x: Ts_predef float (); y: Ts_predef int32# (); y2: Ts_predef int32# (); z: Ts_predef int () })                                                                                                                                                                                                                                                         |
| .57                            | path=record_unboxed_nested/331[11], definition=(Tds_record_mixed { foo: Ts_constr uid=.45 path=record_unboxed_tuple/315[7] (); bar: Ts_predef int () })                                                                                                                                                                                                                                                          |
| .60                            | path=unboxed_variant_record/336[12], definition=(Tds_variant_unboxed name=Bar arg_name=x arg_shape=Ts_predef float# () arg_layout=float64)                                                                                                                                                                                                                                                                       |
| .69                            | path=unboxed_variant_value/339[13], definition=(Tds_variant_unboxed name=Baz arg_name=None arg_shape=Ts_constr uid=.42 path=record_float/310[6] () arg_layout=value)                                                                                                                                                                                                                                             |
| .71                            | path=unboxed_variant_unboxed_float/341[14], definition=(Tds_variant_unboxed name=Baz arg_name=None arg_shape=Ts_predef float# () arg_layout=float64)                                                                                                                                                                                                                                                             |
| .73                            | path=inner_record/343[15], definition=(Tds_record_boxed { x: Ts_predef int (); y: Ts_predef float () })                                                                                                                                                                                                                                                                                                          |
| .74                            | path=outer_record/344[15], definition=(Tds_record_boxed { t: Ts_constr uid=.73 path=inner_record/343[15] (); z: Ts_constr uid=<predef:bool> path=bool/6! () })                                                                                                                                                                                                                                                   |
| .79                            | path=is_void/353[16], definition=(Tds_alias Ts_constr uid=.0 path=t_void/284[1] ())                                                                                                                                                                                                                                                                                                                              |
| .80                            | path=tuple_with_void/354[17], definition=(Tds_alias Ts_unboxed_tuple (Ts_constr uid=.0 path=t_void/284[1] (), Ts_predef int ()))                                                                                                                                                                                                                                                                                 |
| .81                            | path=unboxed_record_with_void/355[18], definition=(Tds_record_unboxed_product { x: Ts_predef int (); y: Ts_constr uid=.0 path=t_void/284[1] () })                                                                                                                                                                                                                                                                |
| <predef:array>                 | path=array/9!, definition=(Tds_other)                                                                                                                                                                                                                                                                                                                                                                            |
| <predef:bool>                  | path=bool/6!, definition=(Tds_variant simple_constructors=false | true complex_constructors=)                                                                                                                                                                                                                                                                                                                    |
| <predef:bytes>                 | path=bytes/3!, definition=(Tds_other)                                                                                                                                                                                                                                                                                                                                                                            |
| <predef:char>                  | path=char/2!, definition=(Tds_other)                                                                                                                                                                                                                                                                                                                                                                             |
| <predef:exn>                   | path=exn/8!, definition=(Tds_other)                                                                                                                                                                                                                                                                                                                                                                              |
| <predef:extension_constructor> | path=extension_constructor/20!, definition=(Tds_other)                                                                                                                                                                                                                                                                                                                                                           |
| <predef:float>                 | path=float/4!, definition=(Tds_other)                                                                                                                                                                                                                                                                                                                                                                            |
| <predef:floatarray>            | path=floatarray/21!, definition=(Tds_other)                                                                                                                                                                                                                                                                                                                                                                      |
| <predef:iarray>                | path=iarray/10!, definition=(Tds_other)                                                                                                                                                                                                                                                                                                                                                                          |
| <predef:int>                   | path=int/1!, definition=(Tds_other)                                                                                                                                                                                                                                                                                                                                                                              |
| <predef:int32>                 | path=int32/16!, definition=(Tds_other)                                                                                                                                                                                                                                                                                                                                                                           |
| <predef:int64>                 | path=int64/17!, definition=(Tds_other)                                                                                                                                                                                                                                                                                                                                                                           |
| <predef:lazy_t>                | path=lazy_t/18!, definition=(Tds_other)                                                                                                                                                                                                                                                                                                                                                                          |
| <predef:lexing_position>       | path=lexing_position/22!, definition=(Tds_record_boxed { pos_fname: Ts_predef string (); pos_lnum: Ts_predef int (); pos_bol: Ts_predef int (); pos_cnum: Ts_predef int () })                                                                                                                                                                                                                                    |
| <predef:list>                  | path=list/11!, definition=(Tds_variant simple_constructors=[] complex_constructors=(:: of Ts_var () * Ts_constr uid=<predef:list> path=list/11! (Ts_var ())))                                                                                                                                                                                                                                                    |
| <predef:nativeint>             | path=nativeint/13!, definition=(Tds_other)                                                                                                                                                                                                                                                                                                                                                                       |
| <predef:option>                | path=option/12!, definition=(Tds_variant simple_constructors=None complex_constructors=(Some of Ts_var ()))                                                                                                                                                                                                                                                                                                      |
| <predef:string>                | path=string/19!, definition=(Tds_other)                                                                                                                                                                                                                                                                                                                                                                          |
| <predef:unit>                  | path=unit/7!, definition=(Tds_variant simple_constructors=() complex_constructors=)                                                                                                                                                                                                                                                                                                                              |
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| UID  | Type                                                                                                                                        | Sort             |
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| .84  | Ts_arrow (Ts_constr uid=.1 path=enum/285[3] (), Ts_constr uid=.1 path=enum/285[3] ())                                                       | value            |
| .85  | Ts_constr uid=.1 path=enum/285[3] ()                                                                                                        | value            |
| .86  | Ts_arrow (Ts_constr uid=.5 path=proper_variant/289[4] (), Ts_constr uid=.5 path=proper_variant/289[4] ())                                   | value            |
| .87  | Ts_constr uid=.5 path=proper_variant/289[4] ()                                                                                              | value            |
| .88  | Ts_arrow (Ts_constr uid=.39 path=record_basic/305[5] (), Ts_constr uid=.39 path=record_basic/305[5] ())                                     | value            |
| .89  | Ts_constr uid=.39 path=record_basic/305[5] ()                                                                                               | value            |
| .90  | Ts_arrow (Ts_constr uid=.42 path=record_float/310[6] (), Ts_constr uid=.42 path=record_float/310[6] ())                                     | value            |
| .91  | Ts_constr uid=.42 path=record_float/310[6] ()                                                                                               | value            |
| .92  | Ts_arrow (Ts_constr uid=.48 path=record_unboxed/318[8] (), Ts_constr uid=.48 path=record_unboxed/318[8] ())                                 | value            |
| .93  | Ts_constr uid=.48 path=record_unboxed/318[8] ()                                                                                             | value            |
| .94  | Ts_arrow (Ts_constr uid=.50 path=record_unboxed_unboxed/320[9] (), Ts_constr uid=.50 path=record_unboxed_unboxed/320[9] ())                 | value            |
| .95  | Ts_constr uid=.50 path=record_unboxed_unboxed/320[9] ()                                                                                     | float64 & bits32 |
| .96  | Ts_arrow (Ts_constr uid=.52 path=record_mixed/322[10] (), Ts_constr uid=.52 path=record_mixed/322[10] ())                                   | value            |
| .97  | Ts_constr uid=.52 path=record_mixed/322[10] ()                                                                                              | value            |
| .98  | Ts_arrow (Ts_constr uid=.57 path=record_unboxed_nested/331[11] (), Ts_constr uid=.57 path=record_unboxed_nested/331[11] ())                 | value            |
| .99  | Ts_constr uid=.57 path=record_unboxed_nested/331[11] ()                                                                                     | value            |
| .100 | Ts_arrow (Ts_constr uid=.60 path=unboxed_variant_record/336[12] (), Ts_constr uid=.60 path=unboxed_variant_record/336[12] ())               | value            |
| .101 | Ts_constr uid=.60 path=unboxed_variant_record/336[12] ()                                                                                    | float64          |
| .102 | Ts_arrow (Ts_constr uid=.69 path=unboxed_variant_value/339[13] (), Ts_constr uid=.69 path=unboxed_variant_value/339[13] ())                 | value            |
| .103 | Ts_constr uid=.69 path=unboxed_variant_value/339[13] ()                                                                                     | value            |
| .104 | Ts_arrow (Ts_constr uid=.71 path=unboxed_variant_unboxed_float/341[14] (), Ts_constr uid=.71 path=unboxed_variant_unboxed_float/341[14] ()) | value            |
| .105 | Ts_constr uid=.71 path=unboxed_variant_unboxed_float/341[14] ()                                                                             | float64          |
| .106 | Ts_arrow (Ts_constr uid=.73 path=inner_record/343[15] (), Ts_constr uid=.73 path=inner_record/343[15] ())                                   | value            |
| .107 | Ts_constr uid=.73 path=inner_record/343[15] ()                                                                                              | value            |
| .108 | Ts_arrow (Ts_constr uid=.74 path=outer_record/344[15] (), Ts_constr uid=.74 path=outer_record/344[15] ())                                   | value            |
| .109 | Ts_constr uid=.74 path=outer_record/344[15] ()                                                                                              | value            |
| .110 | Ts_arrow (Ts_constr uid=.79 path=is_void/353[16] (), Ts_constr uid=.79 path=is_void/353[16] ())                                             | value            |
| .111 | Ts_constr uid=.79 path=is_void/353[16] ()                                                                                                   | void             |
| .112 | Ts_arrow (Ts_constr uid=.80 path=tuple_with_void/354[17] (), Ts_constr uid=.80 path=tuple_with_void/354[17] ())                             | value            |
| .113 | Ts_constr uid=.80 path=tuple_with_void/354[17] ()                                                                                           | void & value     |
| .114 | Ts_arrow (Ts_constr uid=.81 path=unboxed_record_with_void/355[18] (), Ts_constr uid=.81 path=unboxed_record_with_void/355[18] ())           | value            |
| .115 | Ts_constr uid=.81 path=unboxed_record_with_void/355[18] ()                                                                                  | value & void     |
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------

module Declarations :
  sig
    type enum = A | B | C
    type proper_variant =
        D of float
      | E of int
      | F
      | H of { x : int; y : float; }
      | G of { x : int; y : int32#; y2 : int32#; z : bool; a : int32#; }
      | I of { x : float; y : float; }
    type record_basic = { x : int; y : int; }
    type record_float = { x : float; y : float; }
    type record_unboxed_tuple = #{ x : float#; y : int32#; }
    type record_unboxed = { x : int; } [@@unboxed]
    type record_unboxed_unboxed = { x : record_unboxed_tuple; } [@@unboxed]
    type record_mixed = { x : float; y : int32#; y2 : int32#; z : int; }
    type record_unboxed_nested = { foo : record_unboxed_tuple; bar : int; }
    type unboxed_variant_record = Bar of { x : float#; } [@@unboxed]
    type unboxed_variant_value = Baz of record_float [@@unboxed]
    type unboxed_variant_unboxed_float = Baz of float# [@@unboxed]
    type inner_record = { x : int; y : float; }
    and outer_record = { t : inner_record; z : bool; }
    type is_void = t_void
    type tuple_with_void = #(t_void * int)
    type unboxed_record_with_void = #{ x : int; y : t_void; }
    val f_enum : enum -> enum
    val f_proper_variant : proper_variant -> proper_variant
    val f_record_basic : record_basic -> record_basic
    val f_record_float : record_float -> record_float
    val f_record_unboxed : record_unboxed -> record_unboxed
    val f_record_unboxed_unboxed :
      record_unboxed_unboxed -> record_unboxed_unboxed
    val f_record_mixed : record_mixed -> record_mixed
    val f_record_unboxed_nested :
      record_unboxed_nested -> record_unboxed_nested
    val f_unboxed_variant_record :
      unboxed_variant_record -> unboxed_variant_record
    val f_unboxed_variant_value :
      unboxed_variant_value -> unboxed_variant_value
    val f_unboxed_variant_unboxed_float :
      unboxed_variant_unboxed_float -> unboxed_variant_unboxed_float
    val f_inner_record : inner_record -> inner_record
    val f_outer_record : outer_record -> outer_record
    val f_is_void : is_void -> is_void
    val f_tuple_with_void : tuple_with_void -> tuple_with_void
    val f_unboxed_record_with_void :
      unboxed_record_with_void -> unboxed_record_with_void
  end
|}]


module UnsupportedVoidInRecords = struct
  type mixed_record_with_void = { x: t_void; y: int; z: t_void; a : int * bool; b: float# }

  let[@inline never][@locals never] f_mixed_record_with_void (x: mixed_record_with_void) = x

end
[%%expect{|
Line 2, characters 2-91:
2 |   type mixed_record_with_void = { x: t_void; y: int; z: t_void; a : int * bool; b: float# }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "t_void" has layout "void".
       Structures with non-value elements may not yet contain types of this layout.
|}]


module UnsupportedVoidOnlyConstructor = struct
  type void_only_constructor = Bar | Foo of #(t_void * t_void)
  let[@inline never][@locals never] f_void_only_constructor (x: void_only_constructor) = x

end
[%%expect{|
Line 2, characters 35-62:
2 |   type void_only_constructor = Bar | Foo of #(t_void * t_void)
                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "#(t_void * t_void)" has layout "void".
       Structures with non-value elements may not yet contain types of this layout.
|}]



module UnsupportedVoidOnlyRecord = struct
  type void_only_record = { x: t_void; y: t_void }
  let[@inline never][@locals never] f_void_only_record (x: void_only_record) = x

end
[%%expect{|
Line 2, characters 2-50:
2 |   type void_only_record = { x: t_void; y: t_void }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
