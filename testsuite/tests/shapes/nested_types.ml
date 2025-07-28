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
   {<.43>
    "Exn"[extension constructor] -> {<.1>
                                     "lbl_exn"[label] -> <.0>;
                                     };
    "Ext"[extension constructor] -> {<.8>
                                     "lbl_ext"[label] -> <.7>;
                                     };
    "ext"[type] -> <.6>;
    "l"[type] -> Record_boxed { lbl: Predef int ()  };
    "t"[type] -> Variant C of lbl_cstr=Predef int () ;
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
