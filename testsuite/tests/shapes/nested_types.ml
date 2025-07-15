(* TEST
 flags = "-dshape";
 expect;
*)

module M : sig

  exception Exn of { lbl_exn : int }
  type l = { lbl : int }
  type ext = ..
  type ext += Ext of { lbl_ext : int }
  type t = C of { lbl_cstr : int }
end = struct
  exception Exn of { lbl_exn : int }
  type l = { lbl : int }
  type ext = ..
  type ext += Ext of { lbl_ext : int }
  type t = C of { lbl_cstr : int }
end
[%%expect{|
{
 "M"[module] ->
   {<.39>
    "Exn"[extension constructor] -> {<.1>
                                     "lbl_exn"[label] -> <.0>;
                                     };
    "Ext"[extension constructor] -> {<.7>
                                     "lbl_ext"[label] -> <.6>;
                                     };
    "ext"[type] -> <.46> = Tds_other;
    "l"[type] -> <.47> = Tds_record_boxed { lbl: Ts_predef int () };
    "t"[type] ->
      <.48> = Tds_variant simple_constructors= complex_constructors=(C of lbl_cstr=Ts_predef int ());
    };
 }
module M :
  sig
    exception Exn of { lbl_exn : int; }
    type l = { lbl : int; }
    type ext = ..
    type ext += Ext of { lbl_ext : int; }
    type t = C of { lbl_cstr : int; }
  end
|}]
