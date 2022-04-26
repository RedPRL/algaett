val inst_clo : Domain.closure -> Domain.t Lazy.t -> Domain.t
val inst_clo' : Domain.closure -> Domain.t -> Domain.t
val eval : env:Domain.env -> Syntax.t -> Domain.t
val eval_top : Syntax.t -> Domain.t
