-- Code for continuation-based model

type Cont a = a -> Mem -> Answer

type M a = Mem -> Cont () -> Cont a -> Answer

result x mem kx ks = ks x mem

(xm @> f) mem kx ks =
  xm mem kx (\ x mem' -> f x mem' kx ks)

--------

callxc :: (Cont () -> M a) -> M a
callxc f mem kx ks = f kx mem kx ks

withxc :: Cont () -> M a -> M a
withxc kx xm mem kx' ks = xm mem kx ks

--------

type GloState = (Env, Mem)
type Answer = (String, GloState)

obey :: Para -> GloState -> (String, GloState)
obey (Calculate exp) (env, mem) =
  eval exp env mem
    (\ () mem' -> ("***exit in main program***", (env, mem')))
    (\ v mem' -> (print_value v, (env, mem')))
obey (Define def) (env, mem) =
  let x = def_lhs def in
  elab def env mem
    (\ () mem' -> ("***exit in definition***", (env, mem')))
    (\ env' mem' -> (print_defn x (find env' x), (env', mem')))

main = dialog funParser obey (init_env, init_mem)
