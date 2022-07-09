module CS = Syntax
module D = NbE.Domain
module R = Refiner

exception Error of Errors.t

type _ Effect.t += Unleash : CS.bound_name * R.ResolveData.t -> CS.name Effect.t

module type Handler =
sig
  include Refiner.Eff.Handler
  val unleash : CS.bound_name -> Refiner.ResolveData.t -> CS.name
end

module Run (H : Handler) =
struct
  module RunR = R.Eff.Run (H)

  let handler (type a) : a Effect.t -> _ =
    function
    | Unleash (p, rdata) ->
      Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
      Algaeff.Fun.Deep.finally k @@ fun () -> H.unleash p rdata
    | _ -> None

  let run f = Effect.Deep.try_with RunR.run f {effc = handler}
end

module Perform =
struct
  include R.Eff.Perform
  let unleash p data = Effect.perform @@ Unleash (p, data)
end

include Perform

let not_inferable ~tm = raise @@ Error (NotInferable {tm})
let ill_typed ~tm ~tp = raise @@ Error (IllTyped {tm; tp})
