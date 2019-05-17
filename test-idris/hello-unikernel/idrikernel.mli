module Hello (Time : Mirage_time_lwt.S) : sig
  val start : 'a -> unit Lwt.t
end
