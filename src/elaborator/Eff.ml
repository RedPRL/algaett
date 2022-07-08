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

module Handle (H : Handler) =
struct
  let run_refiner f =
    let module Handle = R.Eff.Handle (H) in
    Handle.run f

  let run f =
    Effect.Deep.try_with
      run_refiner f
      { effc =
          fun (type a) (eff : a Effect.t) ->
            match eff with
            | Unleash (p, rdata) ->
              Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
              Algaeff.Fun.Deep.finally k @@ fun () -> H.unleash p rdata
            | _ -> None }
end

module Perform =
struct
  include R.Eff.Perform
  let unleash p data = Effect.perform @@ Unleash (p, data)
end

include Perform

let not_inferable ~tm = raise @@ Error (NotInferable {tm})
let ill_typed ~tm ~tp = raise @@ Error (IllTyped {tm; tp})
