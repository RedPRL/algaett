module CS = Syntax
module D = NbE.Domain
module R = Refiner

type _ Effect.t += Unleash : Asai.Span.t option * CS.bound_name * R.ResolveData.t -> CS.name Effect.t

module type Handler =
sig
  include Refiner.Eff.Handler
  val unleash : ?loc:Asai.Span.t -> CS.bound_name -> Refiner.ResolveData.t -> CS.name
end

module Run (H : Handler) =
struct
  module RunR = R.Eff.Run (H)

  let handler (type a) : a Effect.t -> _ =
    function
    | Unleash (loc, p, rdata) ->
      Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
      Algaeff.Fun.Deep.finally k @@ fun () -> H.unleash ?loc p rdata
    | _ -> None

  let run f = Effect.Deep.try_with RunR.run f {effc = handler}
end

module Perform =
struct
  include R.Eff.Perform
  let unleash ?loc p data = Effect.perform @@ Unleash (loc, p, data)
end

include Perform
