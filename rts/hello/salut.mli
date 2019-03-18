module Unmodul (Time : Mirage_time.S) : sig 
    val pa : int -> string
    val ok : string
    val start : 'a -> unit Lwt.t
end

val ascot : string