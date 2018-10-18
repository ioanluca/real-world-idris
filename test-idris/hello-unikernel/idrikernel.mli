module Hello (Time : Mirage_time_lwt.S) : sig
  val test : int -> int
  val start : 'a -> unit Lwt.t
end
