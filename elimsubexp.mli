val add_common_exp : (KNormal.t * Id.t * int) list -> KNormal.t -> Id.t -> int -> (KNormal.t * Id.t * int) list
val remove_common_exp : (KNormal.t * Id.t * int) list -> int -> (KNormal.t * Id.t * int) list
val find_common_exp : (KNormal.t * Id.t * int) list -> KNormal.t -> Id.t
val g : (KNormal.t * Id.t * int) list ref -> int ->  KNormal.t -> KNormal.t
val f : int -> KNormal.t -> KNormal.t
