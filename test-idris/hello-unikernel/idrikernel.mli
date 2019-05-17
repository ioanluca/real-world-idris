module Hello (Time : Mirage_time_lwt.S) : sig
  val start : unit -> unit Lwt.t
end
