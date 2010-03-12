open Lambdajs_cps

type curr = {
  curr_exp : cpsexp;
  parent : curr option;

}

val cmd_loop : curr -> unit
