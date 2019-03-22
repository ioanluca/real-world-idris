module Main (CON:Conduit_mirage.S) : sig
  val start : CON.t -> unit Lwt.t
end
