val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
val printf : ('a, out_channel, unit) format -> 'a
val eprintf : ('a, out_channel, unit) format -> 'a
val ifprintf : 'a -> ('b, 'a, unit) format -> 'b
val sprintf : ('a, unit, string) format -> 'a
val bprintf : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
val kfprintf :
  (out_channel -> 'a) ->
  out_channel -> ('b, out_channel, unit, 'a) format4 -> 'b
val ksprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
val kbprintf :
  (Buffer.t -> 'a) -> Buffer.t -> ('b, Buffer.t, unit, 'a) format4 -> 'b
val kprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b


val ( @@ ) : ('a -> 'b) -> 'a -> 'b
val ( @. ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val ( @.. ) : ('a -> 'b) -> ('c -> 'd -> 'a) -> 'c -> 'd -> 'b
val ( @... ) : ('a -> 'b) -> ('c -> 'd -> 'e -> 'a) -> 'c -> 'd -> 'e -> 'b
val ( @.... ) :
  ('a -> 'b) -> ('c -> 'd -> 'e -> 'f -> 'a) -> 'c -> 'd -> 'e -> 'f -> 'b
val ( |> ) : 'a -> ('a -> 'b) -> 'b
val ( |>. ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val uncurry3 : ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd
val uncurry4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a * 'b * 'c * 'd -> 'e
val uncurry5 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a * 'b * 'c * 'd * 'e -> 'f
val uncurry6 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  'a * 'b * 'c * 'd * 'e * 'f -> 'g
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val curry3 : ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
val curry4 : ('a * 'b * 'c * 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
val curry5 :
  ('a * 'b * 'c * 'd * 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
val curry6 :
  ('a * 'b * 'c * 'd * 'e * 'f -> 'g) ->
  'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val dup : ('a -> 'a -> 'b) -> 'a -> 'b
val id : 'a -> 'a
val const : 'a -> 'b -> 'a


val some : 'a -> 'a option
val none : 'a -> 'b option
val opt_or : 'a option -> 'a -> 'a
val ( |? ) : 'a option -> 'a -> 'a
val optOr : 'a -> 'a option -> 'a
val optMap : ('a -> 'b) -> 'a option -> 'b option
val maybe : 'a -> ('b -> 'a) -> 'b option -> 'a
val unmaybe : 'a -> ('a -> 'b) -> 'a -> 'b option
val optIf : ('a -> bool) -> ('a -> 'b) -> 'a -> 'b option


val maybeE : 'a -> ('b -> 'a) -> 'b -> 'a
val maybeEx : exn -> 'a -> ('b -> 'a) -> 'b -> 'a
val maybeExl : exn list -> 'a -> ('b -> 'a) -> 'b -> 'a
val maybeEOF : 'a -> ('b -> 'a) -> 'b -> 'a
val maybeNF : 'a -> ('b -> 'a) -> 'b -> 'a
val optE : ('a -> 'b) -> 'a -> 'b option
val optEx : exn -> ('a -> 'b) -> 'a -> 'b option
val optExl : exn list -> ('a -> 'b) -> 'a -> 'b option
val optEOF : ('a -> 'b) -> 'a -> 'b option
val optNF : ('a -> 'b) -> 'a -> 'b option
val finally : ('a -> unit) -> ('a -> 'b) -> 'a -> 'b


val lessThan : 'a -> 'a -> bool
val lessOrEqualTo : 'a -> 'a -> bool
val greaterThan : 'a -> 'a -> bool
val greaterOrEqualTo : 'a -> 'a -> bool
val eq : 'a -> 'a -> bool
val neq : 'a -> 'a -> bool
val equals : 'a -> 'a -> bool
val lt : 'a -> 'a -> bool
val lte : 'a -> 'a -> bool
val gt : 'a -> 'a -> bool
val gte : 'a -> 'a -> bool
val between : 'a -> 'a -> 'a -> bool


val tuple : 'a -> 'b -> 'a * 'b
val tuple3 : 'a -> 'b -> 'c -> 'a * 'b * 'c
val tuple4 : 'a -> 'b -> 'c -> 'd -> 'a * 'b * 'c * 'd
val tuple5 : 'a -> 'b -> 'c -> 'd -> 'e -> 'a * 'b * 'c * 'd * 'e
val tuple6 : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'a * 'b * 'c * 'd * 'e * 'f
val reverseTuple : 'a * 'b -> 'b * 'a
val trev : 'a * 'b -> 'b * 'a
val fuple : ('a -> 'b) -> ('a -> 'c) -> 'a -> 'b * 'c
val fuplel : ('a -> 'b) -> 'a -> 'b * 'a
val fupler : ('a -> 'b) -> 'a -> 'a * 'b


val array : 'a list -> 'a array
val list : 'a array -> 'a list
val int : float -> int
val char : int -> char
val parseInt : string -> int
val parseFloat : string -> float
val showInt : int -> string
val showFloat : float -> string
val charCode : char -> int
val ord : char -> int
val chr : int -> char
val string_of_char : char -> string
val char_of_string : string -> char option


val loop : ('a -> 'b) -> 'a -> 'c
val recurseOpt : ('a -> 'a option) -> 'a -> 'a
val recurseWhile : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a
val recurseUntil : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a
val recurseTo : 'a -> ('a -> 'a) -> 'a -> 'a
val recurseN : ('a -> 'a) -> int -> 'a -> 'a
val unfoldrOpt : ('a -> ('b * 'a) option) -> 'a -> 'b list
val unfoldlOpt : ('a -> ('b * 'a) option) -> 'a -> 'b list
val unfoldr : ('a -> bool) -> ('a -> 'b * 'a) -> 'a -> 'b list
val unfoldl : ('a -> bool) -> ('a -> 'b * 'a) -> 'a -> 'b list
val unfoldrWhile : ('a -> bool) -> ('a -> 'b * 'a) -> 'a -> 'b list
val unfoldlWhile : ('a -> bool) -> ('a -> 'b * 'a) -> 'a -> 'b list
val unfoldrUntil : ('a -> bool) -> ('a -> 'b * 'a) -> 'a -> 'b list
val unfoldlUntil : ('a -> bool) -> ('a -> 'b * 'a) -> 'a -> 'b list
val unfoldrFilter :
  ('a -> bool) -> ('a -> bool) -> ('a -> 'b * 'a) -> 'a -> 'b list
val unfoldlFilter :
  ('a -> bool) -> ('a -> bool) -> ('a -> 'b * 'a) -> 'a -> 'b list
val unfoldlN : ('a -> 'b) -> int -> 'a -> 'b list
val forN : (int -> 'a) -> int -> unit
val generateOpt : ('a -> 'a option) -> 'a -> 'a list
val generate : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a list
val generateUntil : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a list
val generateOptR : ('a -> 'a option) -> 'a -> 'a list
val generateR : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a list
val generateUntilR : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a list
val iterate : ('a -> 'a) -> int -> 'a -> 'a list


val coreCount : unit -> int option
val global_process_count : int ref
val invoke : ('a -> 'b) -> 'a -> unit -> 'b
val par_iter : ?process_count:int -> ('a -> unit) -> 'a list -> unit
val par_map : ?process_count:int -> ('a -> 'b) -> 'a list -> 'b list
val pforN : ?process_count:int -> (int -> 'a) -> int -> unit
val mapReduce :
  ('a -> 'b) -> ('c -> 'b -> 'd) -> 'c -> ('d -> 'e) -> 'a -> 'e


val round : float -> int
val ceiling : float -> float
val quot : float -> int -> int
val recip : float -> float
val signumf : float -> float
val logBase : float -> float -> float
val root : float -> float -> float
val absf : float -> float
val pi : float
val addf : float -> float -> float
val subtractf : float -> float -> float
val multiplyf : float -> float -> float
val dividef : float -> float -> float
val negatef : float -> float
val average2f : float -> float -> float


val average2 : int -> int -> int
val quot_rem : int -> int -> int * int
val rem : int -> int -> int
val even : int -> bool
val odd : int -> bool
val signum : int -> int
val succ : int -> int
val pred : int -> int
val add : int -> int -> int
val subtract : int -> int -> int
val multiply : int -> int -> int
val divide : int -> int -> int
val modulo : int -> int -> int
val negate : int -> int
val gcd : int -> int -> int
val lcm : int -> int -> int


val timeNow : unit -> float
val timeZone : int
val formatTime : ?zone:int -> string -> float -> string
val showTime : float -> string
val showDate : float -> string
val httpDate : float -> string
val second : float
val minute : float
val hour : float
val day : float
val week : float
val month : float
val year : float
val sleep : int -> unit


val groupByName : string -> Unix.group_entry
val groupByGid : int -> Unix.group_entry
val groupGid : string -> int
val groupName : int -> string
val gidMembers : int -> string array
val groupMembers : string -> string array
val userByName : string -> Unix.passwd_entry
val userByUid : int -> Unix.passwd_entry
val userUid : string -> int
val userGid : string -> int
val userGroup : string -> string
val userGecos : string -> string
val userDir : string -> string
val userShell : string -> string
val userName : int -> string


module PreList :
  sig
    val length : 'a list -> int
    val hd : 'a list -> 'a
    val tl : 'a list -> 'a list
    val rev : 'a list -> 'a list
    val append : 'a list -> 'a list -> 'a list
    val rev_append : 'a list -> 'a list -> 'a list
    val concat : 'a list list -> 'a list
    val flatten : 'a list list -> 'a list
    val iter : ('a -> unit) -> 'a list -> unit
    val rev_map : ('a -> 'b) -> 'a list -> 'b list
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
    val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
    val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
    val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
    val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
    val fold_right2 :
      ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
    val for_all : ('a -> bool) -> 'a list -> bool
    val exists : ('a -> bool) -> 'a list -> bool
    val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
    val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
    val mem : 'a -> 'a list -> bool
    val memq : 'a -> 'a list -> bool
    val find : ('a -> bool) -> 'a list -> 'a
    val filter : ('a -> bool) -> 'a list -> 'a list
    val find_all : ('a -> bool) -> 'a list -> 'a list
    val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
    val assoc : 'a -> ('a * 'b) list -> 'b
    val assq : 'a -> ('a * 'b) list -> 'b
    val mem_assoc : 'a -> ('a * 'b) list -> bool
    val mem_assq : 'a -> ('a * 'b) list -> bool
    val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
    val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
    val split : ('a * 'b) list -> 'a list * 'b list
    val combine : 'a list -> 'b list -> ('a * 'b) list
    val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
    val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
    val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
    val reverse : 'a list -> 'a list
    val nth : int -> 'a list -> 'a
    val ( $$ ) : 'a list -> int -> 'a
    val cons : 'a -> 'a list -> 'a list
    val head : 'a list -> 'a
    val tail : 'a list -> 'a list
    val pop : 'a list -> 'a list * 'a
    val popped : 'a list -> 'a list
    val last : 'a list -> 'a
    val first : 'a list -> 'a
    val shift : 'a list -> 'a list * 'a
    val unshift : 'a -> 'a list -> 'a list
    val map : ('a -> 'b) -> 'a list -> 'b list
    val assocBy : ('a -> bool) -> ('a * 'b) list -> 'b
    val lookup : 'a -> ('a * 'b) list -> 'b option
    val lookupBy : ('a -> bool) -> ('a * ('b -> 'c)) list -> 'b -> 'c option
    val len : 'a list -> int
    val all : ('a -> bool) -> 'a list -> bool
    val any : ('a -> bool) -> 'a list -> bool
    val allEqual : 'a list -> bool
    val includes : 'a -> 'a list -> bool
    val has : 'a -> 'a list -> bool
    val elem : 'a -> 'a list -> bool
    val notElem : 'a -> 'a list -> bool
    val indexOf : 'a -> 'a list -> int
    val findIndex : ('a -> bool) -> 'a list -> int
    val findWithIndex : ('a -> bool) -> 'a list -> 'a * int
    val null : 'a list -> bool
    val concatMap : ('a -> 'b list) -> 'a list -> 'b list
    val pick : int list -> 'a list -> 'a list
    val pickWith : ('a -> 'b) list -> 'a -> 'b list
    val span : ('a -> bool) -> 'a list -> 'a list * 'a list
    val break : ('a -> bool) -> 'a list -> 'a list * 'a list
    val takeWhile : ('a -> bool) -> 'a list -> 'a list
    val take : int -> 'a list -> 'a list
    val dropWhile : ('a -> bool) -> 'a list -> 'a list
    val drop : int -> 'a list -> 'a list
    val dropWhile2 :
      ('a -> 'b -> bool) -> 'a list -> 'b list -> 'a list * 'b list
    val drop2 : int -> 'a list -> 'b list -> 'a list * 'b list
    val splitAt : int -> 'a list -> 'a list * 'a list
    val sub : int -> int -> 'a list -> 'a list
    val slice : int -> int -> 'a list -> 'a list
    val interlace : 'a -> 'a list -> 'a list
    val compact : 'a option list -> 'a list
    val squeeze : 'a list -> 'a list
    val sort : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list
    val sortBy : ?cmp:('a -> 'a -> int) -> ('b -> 'a) -> 'b list -> 'b list
    val uniq : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list
    val reject : ('a -> bool) -> 'a list -> 'a list
    val without : 'a -> 'a list -> 'a list
    val neighbours : 'a -> 'a list -> 'a option * 'a option
    val neighbourLists : 'a -> int -> 'a list -> 'a list * 'a list
    val mapWindow : ('a list -> 'b) -> int -> 'a list -> 'b list
    val foldl : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    val foldl1 : ('a -> 'a -> 'a) -> 'a list -> 'a
    val foldr : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
    val foldr1 : ('a -> 'a -> 'a) -> 'a list -> 'a
    val scanl : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a list
    val scanl1 : ('a -> 'a -> 'a) -> 'a list -> 'a list
    val scanr : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a list
    val scanr1 : ('a -> 'a -> 'a) -> 'a list -> 'a list
    val zipWith : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
    val zip : 'a list -> 'b list -> ('a * 'b) list
    val unzip : ('a * 'b) list -> 'a list * 'b list
    val zipWith3 :
      ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list
    val zip3 : 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list
    val unzip3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
    val iterWithIndex : ('a -> int -> 'b) -> 'a list -> unit
    val each : ('a -> unit) -> 'a list -> unit
    val eachWithIndex : ('a -> int -> 'b) -> 'a list -> unit
    val mapWithIndex : ('a -> int -> 'b) -> 'a list -> 'b list
    val diffSorted : 'a list -> 'a list -> 'a list
    val diff : 'a list -> 'a list -> 'a list
    val product : int list -> int
    val productf : float list -> float
    val sum : int list -> int
    val sumf : float list -> float
    val average : int list -> int
    val averagef : float list -> float
    val cycle : int -> 'a list -> 'a list
    val range : int -> int -> int list
    val init : (int -> 'a) -> int -> 'a list
    val step : int -> int -> int -> int list
    val ( -- ) : int -> int -> int list
    val replicate : int -> 'a -> 'a list
    val times : int -> 'a list -> 'a list
    val maximum : 'a list -> 'a
    val maxBy : ('a -> 'b) -> 'a -> 'a -> 'a
    val maximumBy : ('a -> 'b) -> 'a list -> 'a
    val maximumByWith : ('a -> 'b) -> 'a list -> 'a * 'b
    val minimum : 'a list -> 'a
    val minBy : ('a -> 'b) -> 'a -> 'a -> 'a
    val minimumBy : ('a -> 'b) -> 'a list -> 'a
    val minimumByWith : ('a -> 'b) -> 'a list -> 'a * 'b
    val groupsOf : int -> 'a list -> 'a list list
    val splitInto : int -> 'a list -> 'a list list
    val groupBy : ('a -> 'a -> bool) -> 'a list -> 'a list list
    val groupAs : ('a -> 'b) -> 'a list -> 'a list list
    val group : 'a list -> 'a list list
    val count : ('a -> bool) -> 'a list -> int
    val rotate : int -> 'a list -> 'a list
    val par_mapReduce :
      ?process_count:int ->
      combine:('a list -> 'b) -> process:('c list -> 'a) -> 'c list -> 'b
    val pmapReduce :
      ('a list -> 'b) ->
      ('c list -> 'a) -> ?process_count:int -> 'c list -> 'b
    val pfoldl :
      ('a -> 'a -> 'a) ->
      ('a -> 'b -> 'a) -> 'a -> ?process_count:int -> 'b list -> 'a
    val pfoldl1 : ('a -> 'a -> 'a) -> ?process_count:int -> 'a list -> 'a
    val pfoldr :
      ('a -> 'a -> 'a) ->
      ('b -> 'a -> 'a) -> 'a -> ?process_count:int -> 'b list -> 'a
    val pfoldr1 : ('a -> 'a -> 'a) -> ?process_count:int -> 'a list -> 'a
    val piter : ('a -> unit) -> ?process_count:int -> 'a list -> unit
    val pmap : ('a -> 'b) -> ?process_count:int -> 'a list -> 'b list
    val pfilter : ('a -> bool) -> ?process_count:int -> 'a list -> 'a list
    val pfoldlSeqN :
      ?process_count:int ->
      int -> ('a -> 'a -> 'a) -> ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    val pfoldl1SeqN :
      ?process_count:int -> int -> ('a -> 'a -> 'a) -> 'a list -> 'a
    val piterSeqN :
      ?process_count:int ->
      int -> ('a -> unit) -> ('b -> 'a) -> 'b list -> unit
    val pinit : ?process_count:int -> (int -> 'a) -> int -> 'a list
    val pzipWith :
      ?process_count:int -> ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  end

val length : 'a list -> int
val hd : 'a list -> 'a
val tl : 'a list -> 'a list
val rev : 'a list -> 'a list
val append : 'a list -> 'a list -> 'a list
val rev_append : 'a list -> 'a list -> 'a list
val concat : 'a list list -> 'a list
val flatten : 'a list list -> 'a list
val iter : ('a -> unit) -> 'a list -> unit
val rev_map : ('a -> 'b) -> 'a list -> 'b list
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
val for_all : ('a -> bool) -> 'a list -> bool
val exists : ('a -> bool) -> 'a list -> bool
val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val mem : 'a -> 'a list -> bool
val memq : 'a -> 'a list -> bool
val find : ('a -> bool) -> 'a list -> 'a
val filter : ('a -> bool) -> 'a list -> 'a list
val find_all : ('a -> bool) -> 'a list -> 'a list
val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
val assoc : 'a -> ('a * 'b) list -> 'b
val assq : 'a -> ('a * 'b) list -> 'b
val mem_assoc : 'a -> ('a * 'b) list -> bool
val mem_assq : 'a -> ('a * 'b) list -> bool
val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
val combine : 'a list -> 'b list -> ('a * 'b) list
val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
val reverse : 'a list -> 'a list
val nth : int -> 'a list -> 'a
val ( $$ ) : 'a list -> int -> 'a
val cons : 'a -> 'a list -> 'a list
val head : 'a list -> 'a
val tail : 'a list -> 'a list
val pop : 'a list -> 'a list * 'a
val popped : 'a list -> 'a list
val last : 'a list -> 'a
val first : 'a list -> 'a
val shift : 'a list -> 'a list * 'a
val unshift : 'a -> 'a list -> 'a list
val map : ('a -> 'b) -> 'a list -> 'b list
val assocBy : ('a -> bool) -> ('a * 'b) list -> 'b
val lookup : 'a -> ('a * 'b) list -> 'b option
val lookupBy : ('a -> bool) -> ('a * ('b -> 'c)) list -> 'b -> 'c option
val len : 'a list -> int
val all : ('a -> bool) -> 'a list -> bool
val any : ('a -> bool) -> 'a list -> bool
val allEqual : 'a list -> bool
val includes : 'a -> 'a list -> bool
val has : 'a -> 'a list -> bool
val elem : 'a -> 'a list -> bool
val notElem : 'a -> 'a list -> bool
val indexOf : 'a -> 'a list -> int
val findIndex : ('a -> bool) -> 'a list -> int
val findWithIndex : ('a -> bool) -> 'a list -> 'a * int
val null : 'a list -> bool
val concatMap : ('a -> 'b list) -> 'a list -> 'b list
val pick : int list -> 'a list -> 'a list
val pickWith : ('a -> 'b) list -> 'a -> 'b list
val span : ('a -> bool) -> 'a list -> 'a list * 'a list
val break : ('a -> bool) -> 'a list -> 'a list * 'a list
val takeWhile : ('a -> bool) -> 'a list -> 'a list
val take : int -> 'a list -> 'a list
val dropWhile : ('a -> bool) -> 'a list -> 'a list
val drop : int -> 'a list -> 'a list
val dropWhile2 :
  ('a -> 'b -> bool) -> 'a list -> 'b list -> 'a list * 'b list
val drop2 : int -> 'a list -> 'b list -> 'a list * 'b list
val splitAt : int -> 'a list -> 'a list * 'a list
val sub : int -> int -> 'a list -> 'a list
val slice : int -> int -> 'a list -> 'a list
val interlace : 'a -> 'a list -> 'a list
val compact : 'a option list -> 'a list
val squeeze : 'a list -> 'a list
val sort : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list
val sortBy : ?cmp:('a -> 'a -> int) -> ('b -> 'a) -> 'b list -> 'b list
val uniq : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list
val reject : ('a -> bool) -> 'a list -> 'a list
val without : 'a -> 'a list -> 'a list
val neighbours : 'a -> 'a list -> 'a option * 'a option
val neighbourLists : 'a -> int -> 'a list -> 'a list * 'a list
val mapWindow : ('a list -> 'b) -> int -> 'a list -> 'b list
val foldl : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val foldl1 : ('a -> 'a -> 'a) -> 'a list -> 'a
val foldr : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
val foldr1 : ('a -> 'a -> 'a) -> 'a list -> 'a
val scanl : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a list
val scanl1 : ('a -> 'a -> 'a) -> 'a list -> 'a list
val scanr : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a list
val scanr1 : ('a -> 'a -> 'a) -> 'a list -> 'a list
val zipWith : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val zip : 'a list -> 'b list -> ('a * 'b) list
val unzip : ('a * 'b) list -> 'a list * 'b list
val zipWith3 :
  ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list
val zip3 : 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list
val unzip3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
val iterWithIndex : ('a -> int -> 'b) -> 'a list -> unit
val each : ('a -> unit) -> 'a list -> unit
val eachWithIndex : ('a -> int -> 'b) -> 'a list -> unit
val mapWithIndex : ('a -> int -> 'b) -> 'a list -> 'b list
val diffSorted : 'a list -> 'a list -> 'a list
val diff : 'a list -> 'a list -> 'a list
val product : int list -> int
val productf : float list -> float
val sum : int list -> int
val sumf : float list -> float
val average : int list -> int
val averagef : float list -> float
val cycle : int -> 'a list -> 'a list
val range : int -> int -> int list
val init : (int -> 'a) -> int -> 'a list
val step : int -> int -> int -> int list
val replicate : int -> 'a -> 'a list
val times : int -> 'a list -> 'a list
val maximum : 'a list -> 'a
val maxBy : ('a -> 'b) -> 'a -> 'a -> 'a
val maximumBy : ('a -> 'b) -> 'a list -> 'a
val maximumByWith : ('a -> 'b) -> 'a list -> 'a * 'b
val minimum : 'a list -> 'a
val minBy : ('a -> 'b) -> 'a -> 'a -> 'a
val minimumBy : ('a -> 'b) -> 'a list -> 'a
val minimumByWith : ('a -> 'b) -> 'a list -> 'a * 'b
val groupsOf : int -> 'a list -> 'a list list
val splitInto : int -> 'a list -> 'a list list
val groupBy : ('a -> 'a -> bool) -> 'a list -> 'a list list
val groupAs : ('a -> 'b) -> 'a list -> 'a list list
val group : 'a list -> 'a list list
val count : ('a -> bool) -> 'a list -> int
val rotate : int -> 'a list -> 'a list
val par_mapReduce :
  ?process_count:int ->
  combine:('a list -> 'b) -> process:('c list -> 'a) -> 'c list -> 'b
val pmapReduce :
  ('a list -> 'b) -> ('c list -> 'a) -> ?process_count:int -> 'c list -> 'b
val pfoldl :
  ('a -> 'a -> 'a) ->
  ('a -> 'b -> 'a) -> 'a -> ?process_count:int -> 'b list -> 'a
val pfoldl1 : ('a -> 'a -> 'a) -> ?process_count:int -> 'a list -> 'a
val pfoldr :
  ('a -> 'a -> 'a) ->
  ('b -> 'a -> 'a) -> 'a -> ?process_count:int -> 'b list -> 'a
val pfoldr1 : ('a -> 'a -> 'a) -> ?process_count:int -> 'a list -> 'a
val piter : ('a -> unit) -> ?process_count:int -> 'a list -> unit
val pmap : ('a -> 'b) -> ?process_count:int -> 'a list -> 'b list
val pfilter : ('a -> bool) -> ?process_count:int -> 'a list -> 'a list
val pfoldlSeqN :
  ?process_count:int ->
  int -> ('a -> 'a -> 'a) -> ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val pfoldl1SeqN :
  ?process_count:int -> int -> ('a -> 'a -> 'a) -> 'a list -> 'a
val piterSeqN :
  ?process_count:int -> int -> ('a -> unit) -> ('b -> 'a) -> 'b list -> unit
val pinit : ?process_count:int -> (int -> 'a) -> int -> 'a list
val pzipWith :
  ?process_count:int -> ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val ( -- ) : int -> int -> int list
val ( @* ) : 'a list -> int -> 'a list


module PreArray :
  sig
    external length : 'a array -> int = "%array_length"
    external get : 'a array -> int -> 'a = "%array_safe_get"
    external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
    external make : int -> 'a -> 'a array = "caml_make_vect"
    external create : int -> 'a -> 'a array = "caml_make_vect"
    val make_matrix : int -> int -> 'a -> 'a array array
    val create_matrix : int -> int -> 'a -> 'a array array
    val append : 'a array -> 'a array -> 'a array
    val concat : 'a array list -> 'a array
    val copy : 'a array -> 'a array
    val fill : 'a array -> int -> int -> 'a -> unit
    val blit : 'a array -> int -> 'a array -> int -> int -> unit
    val to_list : 'a array -> 'a list
    val of_list : 'a list -> 'a array
    val iteri : (int -> 'a -> unit) -> 'a array -> unit
    val mapi : (int -> 'a -> 'b) -> 'a array -> 'b array
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
    val fold_right : ('a -> 'b -> 'b) -> 'a array -> 'b -> 'b
    val sort : ('a -> 'a -> int) -> 'a array -> unit
    val stable_sort : ('a -> 'a -> int) -> 'a array -> unit
    val fast_sort : ('a -> 'a -> int) -> 'a array -> unit
    external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
    external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"
    val len : 'a array -> int
    val init : (int -> 'a) -> int -> 'a array
    val range : int -> int -> int array
    val reverse : 'a array -> 'a array
    val rev : 'a array -> 'a array
    val normalizeIndex : int -> 'a array -> int
    val times : int -> 'a array -> 'a array
    val iter : ('a -> 'b) -> 'a array -> unit
    val iterWithIndex : ('a -> int -> 'b) -> 'a array -> unit
    val map : ('a -> 'b) -> 'a array -> 'b array
    val mapWithIndex : ('a -> int -> 'b) -> 'a array -> 'b array
    val filter : ('a -> bool) -> 'a array -> 'a array
    val filterWithIndex : ('a -> int -> bool) -> 'a array -> 'a array
    val findWithIndex : ('a -> int -> bool) -> 'a array -> 'a * int
    val find : ('a -> bool) -> 'a array -> 'a
    val findIndex : ('a -> bool) -> 'a array -> int
    val indexOf : 'a -> 'a array -> int
    val zipWith : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
    val map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
    val zipWith3 :
      ('a -> 'b -> 'c -> 'd) -> 'a array -> 'b array -> 'c array -> 'd array
    val map3 :
      ('a -> 'b -> 'c -> 'd) -> 'a array -> 'b array -> 'c array -> 'd array
    val foldl : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
    val foldl1 : ('a -> 'a -> 'a) -> 'a array -> 'a
    val foldr : ('a -> 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldr1 : ('a -> 'a -> 'a) -> 'a array -> 'a
    val maximum : 'a array -> 'a
    val minimum : 'a array -> 'a
    val maximumBy : ('a -> 'b) -> 'a array -> 'a
    val minimumBy : ('a -> 'b) -> 'a array -> 'a
    val sub : int -> int -> 'a array -> 'a array
    val slice_to_sub : int -> int -> 'a array -> int * int
    val slice : int -> int -> 'a array -> 'a array
    val subStride : int -> int -> int -> 'a array -> 'a array
    val first : 'a array -> 'a
    val head : 'a array -> 'a
    val tail : 'a array -> 'a array
    val last : 'a array -> 'a
    val popped : 'a array -> 'a array
    val pop : 'a array -> 'a array * 'a
    val push : 'a -> 'a array -> 'a array
    val shift : 'a array -> 'a array * 'a
    val unshift : 'a -> 'a array -> 'a array
    val take : int -> 'a array -> 'a array
    val takeWhile : ('a -> bool) -> 'a array -> 'a array
    val drop : int -> 'a array -> 'a array
    val dropWhile : ('a -> bool) -> 'a array -> 'a array
    val splitAt : int -> 'a array -> 'a array * 'a array
    val break : ('a -> bool) -> 'a array -> 'a array * 'a array
    val span : ('a -> bool) -> 'a array -> 'a array * 'a array
    val interlace : 'a -> 'a array -> 'a array
    val reject : ('a -> bool) -> 'a array -> 'a array
    val without : 'a -> 'a array -> 'a array
    val groupsOf : int -> 'a array -> 'a array list
    val splitInto : int -> 'a array -> 'a array list
    val iterSub : int -> int -> ('a -> 'b) -> 'a array -> unit
    val iterSlice : int -> int -> ('a -> 'b) -> 'a array -> unit
    val mapSub : int -> int -> ('a -> 'b) -> 'a array -> 'b array
    val mapSlice : int -> int -> ('a -> 'b) -> 'a array -> 'b array
    val foldlSub : int -> int -> ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
    val foldl1Sub : int -> int -> ('a -> 'a -> 'a) -> 'a array -> 'a
    val foldrSub : int -> int -> ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
    val foldr1Sub : int -> int -> ('a -> 'a -> 'a) -> 'a array -> 'a
    val foldlSlice : int -> int -> ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
    val foldl1Slice : int -> int -> ('a -> 'a -> 'a) -> 'a array -> 'a
    val foldrSlice : int -> int -> ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
    val foldr1Slice : int -> int -> ('a -> 'a -> 'a) -> 'a array -> 'a
    val sum : int array -> int
    val sumf : float array -> float
    val product : int array -> int
    val productf : float array -> float
    val average : int array -> int
    val averagef : float array -> float
    val sumSub : int -> int -> int array -> int
    val sumSubf : int -> int -> float array -> float
    val sumSlice : int -> int -> int array -> int
    val sumSlicef : int -> int -> float array -> float
    val productSub : int -> int -> int array -> int
    val productSubf : int -> int -> float array -> float
    val productSlice : int -> int -> int array -> int
    val productSlicef : int -> int -> float array -> float
    val averageSub : int -> int -> int array -> int
    val averageSubf : int -> int -> float array -> float
    val averageSlice : int -> int -> int array -> int
    val averageSlicef : int -> int -> float array -> float
    val pick : int list -> 'a array -> 'a list
    val pickWith : ('a -> 'b) list -> 'a -> 'b list
    val par_mapReduce :
      ?process_count:int ->
      combine:('a list -> 'b) -> process:('c array -> 'a) -> 'c array -> 'b
    val pmapReduce :
      ('a list -> 'b) ->
      ('c array -> 'a) -> ?process_count:int -> 'c array -> 'b
    val pfoldl :
      ('a -> 'a -> 'a) ->
      ('a -> 'b -> 'a) -> 'a -> ?process_count:int -> 'b array -> 'a
    val pfoldl1 : ('a -> 'a -> 'a) -> ?process_count:int -> 'a array -> 'a
    val pfoldr :
      ('a -> 'a -> 'a) ->
      ('b -> 'a -> 'a) -> 'a -> ?process_count:int -> 'b array -> 'a
    val pfoldr1 : ('a -> 'a -> 'a) -> ?process_count:int -> 'a array -> 'a
    val piter : ('a -> 'b) -> ?process_count:int -> 'a array -> unit
    val pmap : ('a -> 'b) -> ?process_count:int -> 'a array -> 'b array
    val pfilter : ('a -> bool) -> ?process_count:int -> 'a array -> 'a array
    val pfoldlSeqN :
      ?process_count:int ->
      int -> ('a -> 'a -> 'a) -> ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
    val piterSeqN :
      ?process_count:int ->
      int -> ('a -> 'b) -> ('c -> 'a) -> 'c array -> unit
    val pinit : ?process_count:int -> (int -> 'a) -> int -> 'a array
    val pzipWith :
      ?process_count:int ->
      ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
  end


module PreString :
  sig
    external length : string -> int = "%string_length"
    external get : string -> int -> char = "%string_safe_get"
    external set : string -> int -> char -> unit = "%string_safe_set"
    external create : int -> string = "caml_create_string"
    val make : int -> char -> string
    val copy : string -> string
    val fill : string -> int -> int -> char -> unit
    val blit : string -> int -> string -> int -> int -> unit
    val concat : string -> string list -> string
    val escaped : string -> string
    val index : string -> char -> int
    val rindex : string -> char -> int
    val index_from : string -> int -> char -> int
    val rindex_from : string -> int -> char -> int
    val contains : string -> char -> bool
    val contains_from : string -> int -> char -> bool
    val rcontains_from : string -> int -> char -> bool
    val uppercase : string -> string
    val lowercase : string -> string
    val capitalize : string -> string
    val uncapitalize : string -> string
    type t = string
    val compare : t -> t -> int
    external unsafe_get : string -> int -> char = "%string_unsafe_get"
    external unsafe_set : string -> int -> char -> unit
      = "%string_unsafe_set"
    external unsafe_blit : string -> int -> string -> int -> int -> unit
      = "caml_blit_string" "noalloc"
    external unsafe_fill : string -> int -> int -> char -> unit
      = "caml_fill_string" "noalloc"
    val init : (int -> char) -> int -> string
    val reverse : string -> string
    val rev : string -> string
    val len : string -> int
    val normalizeIndex : int -> string -> int
    val times : int -> string -> string
    val iter : (char -> 'a) -> string -> unit
    val iterWithIndex : (char -> int -> 'a) -> string -> unit
    val map : (char -> char) -> string -> string
    val mapWithIndex : (char -> int -> char) -> string -> string
    val mapToList : (char -> 'a) -> string -> 'a list
    val mapToArray : (char -> 'a) -> string -> 'a array
    val to_array : string -> char array
    val of_array : char array -> string
    val to_list : string -> char list
    val of_list : char list -> string
    val to_byte_array : string -> int array
    val of_byte_array : int array -> string
    val filter : (char -> bool) -> string -> string
    val filterWithIndex : (char -> int -> bool) -> string -> string
    val findWithIndex : (char -> int -> bool) -> string -> char * int
    val find : (char -> bool) -> string -> char
    val findIndex : (char -> bool) -> string -> int
    val indexOf : char -> string -> int
    val zipWith : (char -> char -> char) -> string -> string -> string
    val map2 : (char -> char -> char) -> string -> string -> string
    val zipWith3 :
      (char -> char -> char -> char) -> string -> string -> string -> string
    val map3 :
      (char -> char -> char -> char) -> string -> string -> string -> string
    val foldl : ('a -> char -> 'a) -> 'a -> string -> 'a
    val foldl1 : (char -> char -> char) -> string -> char
    val foldr : (char -> 'a -> 'a) -> 'a -> string -> 'a
    val foldr1 : (char -> char -> char) -> string -> char
    val maximum : string -> char
    val minimum : string -> char
    val maximumBy : (char -> 'a) -> string -> char
    val minimumBy : (char -> 'a) -> string -> char
    val sub : int -> int -> string -> string
    val slice_to_sub : int -> int -> string -> int * int
    val slice : int -> int -> string -> string
    val subStride : int -> int -> int -> string -> string
    val first : string -> char
    val head : string -> char
    val tail : string -> string
    val last : string -> char
    val popped : string -> string
    val append : string -> string -> string
    val pop : string -> string * char
    val push : char -> string -> string
    val shift : string -> string * char
    val unshift : char -> string -> string
    val take : int -> string -> string
    val takeWhile : (char -> bool) -> string -> string
    val drop : int -> string -> string
    val dropWhile : (char -> bool) -> string -> string
    val splitAt : int -> string -> string * string
    val break : (char -> bool) -> string -> string * string
    val span : (char -> bool) -> string -> string * string
    val interlace : char -> string -> string
    val reject : (char -> bool) -> string -> string
    val without : char -> string -> string
    val groupsOf : int -> string -> string list
    val splitInto : int -> string -> string list
    val iterSub : int -> int -> (char -> 'a) -> string -> unit
    val iterSlice : int -> int -> (char -> 'a) -> string -> unit
    val mapSub : int -> int -> (char -> char) -> string -> string
    val mapSlice : int -> int -> (char -> char) -> string -> string
    val foldlSub : int -> int -> ('a -> char -> 'a) -> 'a -> string -> 'a
    val foldl1Sub : int -> int -> (char -> char -> char) -> string -> char
    val foldrSub : int -> int -> ('a -> char -> 'a) -> 'a -> string -> 'a
    val foldr1Sub : int -> int -> (char -> char -> char) -> string -> char
    val foldlSlice : int -> int -> ('a -> char -> 'a) -> 'a -> string -> 'a
    val foldl1Slice : int -> int -> (char -> char -> char) -> string -> char
    val foldrSlice : int -> int -> ('a -> char -> 'a) -> 'a -> string -> 'a
    val foldr1Slice : int -> int -> (char -> char -> char) -> string -> char
    val add_int : int -> char -> int
    val add_float : float -> char -> float
    val mul_int : int -> char -> int
    val mul_float : float -> char -> float
    val sum : string -> int
    val sumf : string -> float
    val product : string -> int
    val productf : string -> float
    val average : string -> int
    val averagef : string -> float
    val sumSub : int -> int -> string -> int
    val sumSubf : int -> int -> string -> float
    val sumSlice : int -> int -> string -> int
    val sumSlicef : int -> int -> string -> float
    val productSub : int -> int -> string -> int
    val productSubf : int -> int -> string -> float
    val productSlice : int -> int -> string -> int
    val productSlicef : int -> int -> string -> float
    val averageSub : int -> int -> string -> int
    val averageSubf : int -> int -> string -> float
    val averageSlice : int -> int -> string -> int
    val averageSlicef : int -> int -> string -> float
    val pick : int list -> string -> char list
    val pickWith : ('a -> 'b) list -> 'a -> 'b list
    val strip :
      ?iflags:Pcre.irflag ->
      ?flags:Pcre.rflag list ->
      ?pat:string ->
      ?pos:int ->
      ?itempl:Pcre.substitution -> ?callout:Pcre.callout -> string -> string
    val split : ?n:int -> string -> string -> string list
    val rsplit : ?n:int -> string -> string -> string list
    val nsplit : string -> int -> string -> string list
    val nrsplit : string -> int -> string -> string list
    val rx :
      ?study:bool ->
      ?limit:int ->
      ?iflags:Pcre.icflag ->
      ?flags:Pcre.cflag list ->
      ?chtables:Pcre.chtables -> string -> Pcre.regexp
    val rex :
      ?study:bool ->
      ?limit:int ->
      ?iflags:Pcre.icflag ->
      ?flags:Pcre.cflag list ->
      ?chtables:Pcre.chtables -> string -> Pcre.regexp
    val escape_rex : string -> string
    val rexsplit : ?n:int -> Pcre.regexp -> string -> string list
    val rexrsplit : ?n:int -> Pcre.regexp -> string -> string list
    val xsplit : ?n:int -> string -> string -> string list
    val xrsplit : ?n:int -> string -> string -> string list
    val xnsplit : string -> int -> string -> string list
    val xnrsplit : string -> int -> string -> string list
    val rexscan : Pcre.regexp -> string -> string list list
    val scan : string -> string -> string list list
    val rexscan_nth : Pcre.regexp -> int -> string -> string list
    val scan_nth : string -> int -> string -> string list
    val xfind : string -> string -> string
    val xfindOpt : string -> string -> string option
    val smatch :
      string ->
      ?iflags:Pcre.irflag ->
      ?flags:Pcre.rflag list ->
      ?rex:Pcre.regexp -> ?pos:int -> ?callout:Pcre.callout -> string -> bool
    val rexmatch :
      Pcre.regexp ->
      ?iflags:Pcre.irflag ->
      ?flags:Pcre.rflag list ->
      ?pat:string -> ?pos:int -> ?callout:Pcre.callout -> string -> bool
    val xmatch :
      string ->
      ?iflags:Pcre.irflag ->
      ?flags:Pcre.rflag list ->
      ?pat:string -> ?pos:int -> ?callout:Pcre.callout -> string -> bool
    val replace :
      string ->
      string ->
      ?iflags:Pcre.irflag ->
      ?flags:Pcre.rflag list ->
      ?rex:Pcre.regexp ->
      ?pos:int ->
      ?itempl:Pcre.substitution -> ?callout:Pcre.callout -> string -> string
    val rexreplace :
      Pcre.regexp ->
      string ->
      ?iflags:Pcre.irflag ->
      ?flags:Pcre.rflag list ->
      ?pat:string ->
      ?pos:int ->
      ?itempl:Pcre.substitution -> ?callout:Pcre.callout -> string -> string
    val xreplace :
      string ->
      string ->
      ?iflags:Pcre.irflag ->
      ?flags:Pcre.rflag list ->
      ?pat:string ->
      ?pos:int ->
      ?itempl:Pcre.substitution -> ?callout:Pcre.callout -> string -> string
    val frexreplace : (string -> string) -> Pcre.regexp -> string -> string
    val fxreplace : (string -> string) -> string -> string -> string
    val quote : string -> string -> string -> string
    val join : string -> string list -> string
    val join_array : string -> string array -> string
    val xreplaceMulti : (string * string) list -> string -> string
    val replaceMulti : (string * string) list -> string -> string
    val words : string -> string list
    val unwords : string list -> string
    val lines : string -> string list
    val unlines : string list -> string
    val rexsplitPartition :
      Pcre.regexp -> string -> (string * string) list * string option
    val xsplitPartition :
      string -> string -> (string * string) list * string option
    val par_mapReduce :
      ?process_count:int ->
      combine:('a list -> 'b) -> process:(string -> 'a) -> string -> 'b
    val pmapReduce :
      ('a list -> 'b) -> (string -> 'a) -> ?process_count:int -> string -> 'b
    val pfoldl :
      ('a -> 'a -> 'a) ->
      ('a -> char -> 'a) -> 'a -> ?process_count:int -> string -> 'a
    val pfoldl1 :
      (char -> char -> char) -> ?process_count:int -> string -> char
    val pfoldr :
      ('a -> 'a -> 'a) ->
      (char -> 'a -> 'a) -> 'a -> ?process_count:int -> string -> 'a
    val pfoldr1 :
      (char -> char -> char) -> ?process_count:int -> string -> char
    val piter : (char -> 'a) -> ?process_count:int -> string -> unit
    val pmap : (char -> char) -> ?process_count:int -> string -> string
    val pfilter : (char -> bool) -> ?process_count:int -> string -> string
    val pfoldlSeqN :
      ?process_count:int ->
      int -> ('a -> 'a -> 'a) -> ('a -> char -> 'a) -> 'a -> string -> 'a
    val piterSeqN :
      ?process_count:int ->
      int -> (char -> 'a) -> (char -> char) -> string -> unit
    val pinit : ?process_count:int -> (int -> char) -> int -> string
    val pzipWith :
      ?process_count:int ->
      (char -> char -> char) -> string -> string -> string
  end


module Bytestring :
  sig
    external length : string -> int = "%string_length"
    external get : string -> int -> char = "%string_safe_get"
    external set : string -> int -> char -> unit = "%string_safe_set"
    external create : int -> string = "caml_create_string"
    val copy : string -> string
    val fill : string -> int -> int -> char -> unit
    val blit : string -> int -> string -> int -> int -> unit
    val escaped : string -> string
    val index : string -> char -> int
    val rindex : string -> char -> int
    val index_from : string -> int -> char -> int
    val rindex_from : string -> int -> char -> int
    val contains : string -> char -> bool
    val contains_from : string -> int -> char -> bool
    val rcontains_from : string -> int -> char -> bool
    val uppercase : string -> string
    val lowercase : string -> string
    val capitalize : string -> string
    val uncapitalize : string -> string
    type t = string
    val compare : t -> t -> int
    external unsafe_blit : string -> int -> string -> int -> int -> unit
      = "caml_blit_string" "noalloc"
    external unsafe_fill : string -> int -> int -> char -> unit
      = "caml_fill_string" "noalloc"
    val reverse : string -> string
    val rev : string -> string
    val len : string -> int
    val normalizeIndex : int -> string -> int
    val times : int -> string -> string
    val mapToList : (char -> 'a) -> string -> 'a list
    val mapToArray : (char -> 'a) -> string -> 'a array
    val to_byte_array : string -> int array
    val of_byte_array : int array -> string
    val append : string -> string -> string
    val strip :
      ?iflags:Pcre.irflag ->
      ?flags:Pcre.rflag list ->
      ?pat:string ->
      ?pos:int ->
      ?itempl:Pcre.substitution -> ?callout:Pcre.callout -> string -> string
    val split : ?n:int -> string -> string -> string list
    val rsplit : ?n:int -> string -> string -> string list
    val nsplit : string -> int -> string -> string list
    val nrsplit : string -> int -> string -> string list
    val rx :
      ?study:bool ->
      ?limit:int ->
      ?iflags:Pcre.icflag ->
      ?flags:Pcre.cflag list ->
      ?chtables:Pcre.chtables -> string -> Pcre.regexp
    val rex :
      ?study:bool ->
      ?limit:int ->
      ?iflags:Pcre.icflag ->
      ?flags:Pcre.cflag list ->
      ?chtables:Pcre.chtables -> string -> Pcre.regexp
    val escape_rex : string -> string
    val rexsplit : ?n:int -> Pcre.regexp -> string -> string list
    val rexrsplit : ?n:int -> Pcre.regexp -> string -> string list
    val xsplit : ?n:int -> string -> string -> string list
    val xrsplit : ?n:int -> string -> string -> string list
    val xnsplit : string -> int -> string -> string list
    val xnrsplit : string -> int -> string -> string list
    val rexscan : Pcre.regexp -> string -> string list list
    val scan : string -> string -> string list list
    val rexscan_nth : Pcre.regexp -> int -> string -> string list
    val scan_nth : string -> int -> string -> string list
    val xfind : string -> string -> string
    val xfindOpt : string -> string -> string option
    val smatch :
      string ->
      ?iflags:Pcre.irflag ->
      ?flags:Pcre.rflag list ->
      ?rex:Pcre.regexp -> ?pos:int -> ?callout:Pcre.callout -> string -> bool
    val rexmatch :
      Pcre.regexp ->
      ?iflags:Pcre.irflag ->
      ?flags:Pcre.rflag list ->
      ?pat:string -> ?pos:int -> ?callout:Pcre.callout -> string -> bool
    val xmatch :
      string ->
      ?iflags:Pcre.irflag ->
      ?flags:Pcre.rflag list ->
      ?pat:string -> ?pos:int -> ?callout:Pcre.callout -> string -> bool
    val replace :
      string ->
      string ->
      ?iflags:Pcre.irflag ->
      ?flags:Pcre.rflag list ->
      ?rex:Pcre.regexp ->
      ?pos:int ->
      ?itempl:Pcre.substitution -> ?callout:Pcre.callout -> string -> string
    val rexreplace :
      Pcre.regexp ->
      string ->
      ?iflags:Pcre.irflag ->
      ?flags:Pcre.rflag list ->
      ?pat:string ->
      ?pos:int ->
      ?itempl:Pcre.substitution -> ?callout:Pcre.callout -> string -> string
    val xreplace :
      string ->
      string ->
      ?iflags:Pcre.irflag ->
      ?flags:Pcre.rflag list ->
      ?pat:string ->
      ?pos:int ->
      ?itempl:Pcre.substitution -> ?callout:Pcre.callout -> string -> string
    val frexreplace : (string -> string) -> Pcre.regexp -> string -> string
    val fxreplace : (string -> string) -> string -> string -> string
    val quote : string -> string -> string -> string
    val join : string -> string list -> string
    val join_array : string -> string array -> string
    val xreplaceMulti : (string * string) list -> string -> string
    val replaceMulti : (string * string) list -> string -> string
    val words : string -> string list
    val unwords : string list -> string
    val lines : string -> string list
    val unlines : string list -> string
    val rexsplitPartition :
      Pcre.regexp -> string -> (string * string) list * string option
    val xsplitPartition :
      string -> string -> (string * string) list * string option
    val unsafe_get : string -> int -> int
    val unsafe_set : string -> int -> int -> unit
    val make : int -> int -> string
    val init : (int -> int) -> int -> string
    val iter : (int -> 'a) -> string -> unit
    val iterWithIndex : (int -> int -> 'a) -> string -> unit
    val map : (int -> int) -> string -> string
    val mapWithIndex : (int -> int -> int) -> string -> string
    val to_array : string -> int array
    val of_array : int array -> string
    val to_list : string -> int list
    val of_list : int list -> string
    val filter : (int -> bool) -> string -> string
    val filterWithIndex : (int -> int -> bool) -> string -> string
    val findWithIndex : (int -> int -> bool) -> string -> int * int
    val find : (int -> bool) -> string -> int
    val findIndex : (int -> bool) -> string -> int
    val indexOf : int -> string -> int
    val zipWith : (int -> int -> int) -> string -> string -> string
    val map2 : (int -> int -> int) -> string -> string -> string
    val zipWith3 :
      (int -> int -> int -> int) -> string -> string -> string -> string
    val map3 :
      (int -> int -> int -> int) -> string -> string -> string -> string
    val foldl : ('a -> int -> 'a) -> 'a -> string -> 'a
    val foldl1 : (int -> int -> int) -> string -> int
    val foldr : (int -> 'a -> 'a) -> 'a -> string -> 'a
    val foldr1 : (int -> int -> int) -> string -> int
    val maximum : string -> int
    val minimum : string -> int
    val maximumBy : (int -> 'a) -> string -> int
    val minimumBy : (int -> 'a) -> string -> int
    val sub : int -> int -> string -> string
    val slice_to_sub : int -> int -> string -> int * int
    val slice : int -> int -> string -> string
    val subStride : int -> int -> int -> string -> string
    val first : string -> int
    val head : string -> int
    val tail : string -> string
    val last : string -> int
    val popped : string -> string
    val pop : string -> string * int
    val push : int -> string -> string
    val shift : string -> string * int
    val unshift : int -> string -> string
    val take : int -> string -> string
    val takeWhile : (int -> bool) -> string -> string
    val drop : int -> string -> string
    val dropWhile : (int -> bool) -> string -> string
    val splitAt : int -> string -> string * string
    val break : (int -> bool) -> string -> string * string
    val span : (int -> bool) -> string -> string * string
    val interlace : int -> string -> string
    val reject : (int -> bool) -> string -> string
    val without : int -> string -> string
    val groupsOf : int -> string -> string list
    val splitInto : int -> string -> string list
    val iterSub : int -> int -> (int -> 'a) -> string -> unit
    val iterSlice : int -> int -> (int -> 'a) -> string -> unit
    val mapSub : int -> int -> (int -> int) -> string -> string
    val mapSlice : int -> int -> (int -> int) -> string -> string
    val foldlSub : int -> int -> ('a -> int -> 'a) -> 'a -> string -> 'a
    val foldl1Sub : int -> int -> (int -> int -> int) -> string -> int
    val foldrSub : int -> int -> ('a -> int -> 'a) -> 'a -> string -> 'a
    val foldr1Sub : int -> int -> (int -> int -> int) -> string -> int
    val foldlSlice : int -> int -> ('a -> int -> 'a) -> 'a -> string -> 'a
    val foldl1Slice : int -> int -> (int -> int -> int) -> string -> int
    val foldrSlice : int -> int -> ('a -> int -> 'a) -> 'a -> string -> 'a
    val foldr1Slice : int -> int -> (int -> int -> int) -> string -> int
    val add_int : int -> int -> int
    val add_float : float -> int -> float
    val mul_int : int -> int -> int
    val mul_float : float -> int -> float
    val sum : string -> int
    val sumf : string -> float
    val product : string -> int
    val productf : string -> float
    val average : string -> int
    val averagef : string -> float
    val sumSub : int -> int -> string -> int
    val sumSubf : int -> int -> string -> float
    val sumSlice : int -> int -> string -> int
    val sumSlicef : int -> int -> string -> float
    val productSub : int -> int -> string -> int
    val productSubf : int -> int -> string -> float
    val productSlice : int -> int -> string -> int
    val productSlicef : int -> int -> string -> float
    val averageSub : int -> int -> string -> int
    val averageSubf : int -> int -> string -> float
    val averageSlice : int -> int -> string -> int
    val averageSlicef : int -> int -> string -> float
    val pick : int list -> string -> int list
    val pickWith : ('a -> 'b) list -> 'a -> 'b list
    val concat : string list -> string
    val par_mapReduce :
      ?process_count:int ->
      combine:('a list -> 'b) -> process:(string -> 'a) -> string -> 'b
    val pmapReduce :
      ('a list -> 'b) -> (string -> 'a) -> ?process_count:int -> string -> 'b
    val pfoldl :
      ('a -> 'a -> 'a) ->
      ('a -> int -> 'a) -> 'a -> ?process_count:int -> string -> 'a
    val pfoldl1 : (int -> int -> int) -> ?process_count:int -> string -> int
    val pfoldr :
      ('a -> 'a -> 'a) ->
      (int -> 'a -> 'a) -> 'a -> ?process_count:int -> string -> 'a
    val pfoldr1 : (int -> int -> int) -> ?process_count:int -> string -> int
    val piter : (int -> 'a) -> ?process_count:int -> string -> unit
    val pmap : (int -> int) -> ?process_count:int -> string -> string
    val pfilter : (int -> bool) -> ?process_count:int -> string -> string
    val pfoldlSeqN :
      ?process_count:int ->
      int -> ('a -> 'a -> 'a) -> ('a -> int -> 'a) -> 'a -> string -> 'a
    val piterSeqN :
      ?process_count:int ->
      int -> (int -> 'a) -> (int -> int) -> string -> unit
    val pinit : ?process_count:int -> (int -> int) -> int -> string
    val pzipWith :
      ?process_count:int -> (int -> int -> int) -> string -> string -> string
  end


module Range :
  sig
    val create : 'a -> 'b -> 'a * 'b
    val length : int * int -> int
    val len : int * int -> int
    val to_list : int * int -> int list
    val to_array : int * int -> int array
    val iter : (int -> 'a) -> int * int -> unit
    val map : (int -> 'a) -> int * int -> 'a list
    val zipWith : (int -> int -> 'a) -> int * int -> int * int -> 'a list
    val map2 : (int -> int -> 'a) -> int * int -> int * int -> 'a list
    val zipWith3 :
      (int -> int -> int -> 'a) ->
      int * int -> int * int -> int * int -> 'a list
    val map3 :
      (int -> int -> int -> 'a) ->
      int * int -> int * int -> int * int -> 'a list
    val left_succ : int * 'a -> int * 'a
    val left_pred : int * 'a -> int * 'a
    val right_succ : 'a * int -> 'a * int
    val right_pred : 'a * int -> 'a * int
    val foldl : ('a -> int -> 'a) -> 'a -> int * int -> 'a
    val foldl1 : (int -> int -> int) -> int * int -> int
    val foldr : (int -> 'a -> 'a) -> 'a -> int * int -> 'a
    val foldr1 : (int -> int -> int) -> int * int -> int
    val find : (int -> bool) -> int * int -> bool
    val filter : (int -> bool) -> int * int -> int list
    val groupsOf : int -> int * int -> (int * int) list
    val splitInto : int -> int * int -> (int * int) list
    val par_mapReduce :
      ?process_count:int ->
      combine:('a list -> 'b) -> process:(int * int -> 'a) -> int * int -> 'b
    val pmapReduce :
      ('a list -> 'b) ->
      (int * int -> 'a) -> ?process_count:int -> int * int -> 'b
    val pfoldl :
      ('a -> 'a -> 'a) ->
      ('a -> int -> 'a) -> 'a -> ?process_count:int -> int * int -> 'a
    val pfoldl1 :
      (int -> int -> int) -> ?process_count:int -> int * int -> int
    val pfoldr :
      ('a -> 'a -> 'a) ->
      (int -> 'a -> 'a) -> 'a -> ?process_count:int -> int * int -> 'a
    val pfoldr1 :
      (int -> int -> int) -> ?process_count:int -> int * int -> int
    val piter : (int -> 'a) -> ?process_count:int -> int * int -> unit
    val pmap : (int -> 'a) -> ?process_count:int -> int * int -> 'a list
    val pfilter :
      (int -> bool) -> ?process_count:int -> int * int -> int list
    val pfoldlSeqN :
      ?process_count:int ->
      int -> ('a -> 'a -> 'a) -> ('a -> int -> 'a) -> 'a -> int * int -> 'a
    val piterSeqN :
      ?process_count:int ->
      int -> ('a -> unit) -> (int -> 'a) -> int * int -> unit
    val pzipWith :
      ?process_count:int ->
      (int -> int -> 'a) -> int * int -> int * int -> 'a list
  end
val ( --> ) : 'a -> 'b -> 'a * 'b


val amake : int -> 'a -> 'a array
val acreate : int -> 'a -> 'a array
val ainit : (int -> 'a) -> int -> 'a array
val alen : 'a array -> int
val aconcat : 'a array list -> 'a array
val areverse : 'a -> 'b array -> 'b array
val arev : 'a -> 'b array -> 'b array
val amap : ('a -> 'b) -> 'a array -> 'b array
val amapSub : int -> int -> ('a -> 'b) -> 'a array -> 'b array
val amapSlice : int -> int -> ('a -> 'b) -> 'a array -> 'b array
val amapWithIndex : ('a -> int -> 'b) -> 'a array -> 'b array
val aiter : ('a -> 'b) -> 'a array -> unit
val aiterSub : int -> int -> ('a -> 'b) -> 'a array -> unit
val aiterSlice : int -> int -> ('a -> 'b) -> 'a array -> unit
val aiterWithIndex : ('a -> int -> 'b) -> 'a array -> unit
val afilter : ('a -> bool) -> 'a array -> 'a array
val afilterWithIndex : ('a -> int -> bool) -> 'a array -> 'a array
val afind : ('a -> bool) -> 'a array -> 'a
val afindWithIndex : ('a -> int -> bool) -> 'a array -> 'a * int
val afindIndex : ('a -> bool) -> 'a array -> int
val afoldl : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
val afoldl1 : ('a -> 'a -> 'a) -> 'a array -> 'a
val afoldlSub : int -> int -> ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
val afoldl1Sub : int -> int -> ('a -> 'a -> 'a) -> 'a array -> 'a
val afoldlSlice : int -> int -> ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
val afoldl1Slice : int -> int -> ('a -> 'a -> 'a) -> 'a array -> 'a
val afoldr : ('a -> 'b -> 'b) -> 'b -> 'a array -> 'b
val afoldr1 : ('a -> 'a -> 'a) -> 'a array -> 'a
val afoldrSub : int -> int -> ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
val afoldr1Sub : int -> int -> ('a -> 'a -> 'a) -> 'a array -> 'a
val afoldrSlice : int -> int -> ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
val afoldr1Slice : int -> int -> ('a -> 'a -> 'a) -> 'a array -> 'a
val asum : int array -> int
val asumSub : int -> int -> int array -> int
val asumSlice : int -> int -> int array -> int
val asumf : float array -> float
val asumSubf : int -> int -> float array -> float
val asumSlicef : int -> int -> float array -> float
val aproduct : int array -> int
val aproductSub : int -> int -> int array -> int
val aproductSlice : int -> int -> int array -> int
val aproductf : float array -> float
val aproductSubf : int -> int -> float array -> float
val aproductSlicef : int -> int -> float array -> float
val aaverage : int array -> int
val aaverageSub : int -> int -> int array -> int
val aaverageSlice : int -> int -> int array -> int
val aaveragef : float array -> float
val aaverageSubf : int -> int -> float array -> float
val aaverageSlicef : int -> int -> float array -> float
val arange : int -> int -> int array
val azipWith : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
val amap2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
val azipWith3 :
  ('a -> 'b -> 'c -> 'd) -> 'a array -> 'b array -> 'c array -> 'd array
val amap3 :
  ('a -> 'b -> 'c -> 'd) -> 'a array -> 'b array -> 'c array -> 'd array
val asub : int -> int -> 'a array -> 'a array
val aslice : int -> int -> 'a array -> 'a array
val asubStride : int -> int -> int -> 'a array -> 'a array
val agroupsOf : int -> 'a array -> 'a array list
val asplitInto : int -> 'a array -> 'a array list
val atimes : int -> 'a array -> 'a array
val apick : int list -> 'a array -> 'a list
val apickWith : ('a -> 'b) list -> 'a -> 'b list
val apmap : ('a -> 'b) -> ?process_count:int -> 'a array -> 'b array
val apiter : ('a -> 'b) -> ?process_count:int -> 'a array -> unit
val apinit : ?process_count:int -> (int -> 'a) -> int -> 'a array
val apzipWith :
  ?process_count:int -> ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
val apfilter : ('a -> bool) -> ?process_count:int -> 'a array -> 'a array
val apfoldl :
  ('a -> 'a -> 'a) ->
  ('a -> 'b -> 'a) -> 'a -> ?process_count:int -> 'b array -> 'a
val apfoldl1 : ('a -> 'a -> 'a) -> ?process_count:int -> 'a array -> 'a
val apfoldr :
  ('a -> 'a -> 'a) ->
  ('b -> 'a -> 'a) -> 'a -> ?process_count:int -> 'b array -> 'a
val apfoldr1 : ('a -> 'a -> 'a) -> ?process_count:int -> 'a array -> 'a
val apfoldlSeqN :
  ?process_count:int ->
  int -> ('a -> 'a -> 'a) -> ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
val apiterSeqN :
  ?process_count:int -> int -> ('a -> 'b) -> ('c -> 'a) -> 'c array -> unit
val ( @| ) : 'a array -> 'a array -> 'a array
val ( @|* ) : int -> 'a array -> 'a array
val ( --| ) : int -> int -> int array


val smake : int -> char -> string
val screate : int -> string
val sinit : (int -> char) -> int -> string
val slen : string -> int
val sconcat : string -> string list -> string
val sreverse : 'a -> string -> string
val srev : 'a -> 'b array -> 'b array
val smap : (char -> char) -> string -> string
val smapSub : int -> int -> (char -> char) -> string -> string
val smapSlice : int -> int -> (char -> char) -> string -> string
val smapWithIndex : (char -> int -> char) -> string -> string
val siter : (char -> 'a) -> string -> unit
val siterSub : int -> int -> (char -> 'a) -> string -> unit
val siterSlice : int -> int -> (char -> 'a) -> string -> unit
val siterWithIndex : (char -> int -> 'a) -> string -> unit
val sfilter : (char -> bool) -> string -> string
val sfilterWithIndex : (char -> int -> bool) -> string -> string
val sfind : (char -> bool) -> string -> char
val sfindWithIndex : (char -> int -> bool) -> string -> char * int
val sfindIndex : (char -> bool) -> string -> int
val sfoldl : ('a -> char -> 'a) -> 'a -> string -> 'a
val sfoldl1 : (char -> char -> char) -> string -> char
val sfoldlSub : int -> int -> ('a -> char -> 'a) -> 'a -> string -> 'a
val sfoldl1Sub : int -> int -> (char -> char -> char) -> string -> char
val sfoldlSlice : int -> int -> ('a -> char -> 'a) -> 'a -> string -> 'a
val sfoldl1Slice : int -> int -> (char -> char -> char) -> string -> char
val sfoldr : (char -> 'a -> 'a) -> 'a -> string -> 'a
val sfoldr1 : (char -> char -> char) -> string -> char
val sfoldrSub : int -> int -> ('a -> char -> 'a) -> 'a -> string -> 'a
val sfoldr1Sub : int -> int -> (char -> char -> char) -> string -> char
val sfoldrSlice : int -> int -> ('a -> char -> 'a) -> 'a -> string -> 'a
val sfoldr1Slice : int -> int -> (char -> char -> char) -> string -> char
val ssum : string -> int
val ssumSub : int -> int -> string -> int
val ssumSlice : int -> int -> string -> int
val ssumf : string -> float
val ssumSubf : int -> int -> string -> float
val ssumSlicef : int -> int -> string -> float
val sproduct : string -> int
val sproductSub : int -> int -> string -> int
val sproductSlice : int -> int -> string -> int
val sproductf : string -> float
val sproductSubf : int -> int -> string -> float
val sproductSlicef : int -> int -> string -> float
val saverage : string -> int
val saverageSub : int -> int -> string -> int
val saverageSlice : int -> int -> string -> int
val saveragef : string -> float
val saverageSubf : int -> int -> string -> float
val saverageSlicef : int -> int -> string -> float
val szipWith : (char -> char -> char) -> string -> string -> string
val smap2 : (char -> char -> char) -> string -> string -> string
val szipWith3 :
  (char -> char -> char -> char) -> string -> string -> string -> string
val smap3 :
  (char -> char -> char -> char) -> string -> string -> string -> string
val ssub : int -> int -> string -> string
val sslice : int -> int -> string -> string
val ssubStride : int -> int -> int -> string -> string
val sgroupsOf : int -> string -> string list
val ssplitInto : int -> string -> string list
val stimes : int -> string -> string
val spick : int list -> string -> char list
val spickWith : ('a -> 'b) list -> 'a -> 'b list
val spmap : (char -> char) -> ?process_count:int -> string -> string
val spiter : (char -> 'a) -> ?process_count:int -> string -> unit
val spinit : ?process_count:int -> (int -> char) -> int -> string
val spzipWith :
  ?process_count:int -> (char -> char -> char) -> string -> string -> string
val spfilter : (char -> bool) -> ?process_count:int -> string -> string
val spfoldl :
  ('a -> 'a -> 'a) ->
  ('a -> char -> 'a) -> 'a -> ?process_count:int -> string -> 'a
val spfoldl1 : (char -> char -> char) -> ?process_count:int -> string -> char
val spfoldr :
  ('a -> 'a -> 'a) ->
  (char -> 'a -> 'a) -> 'a -> ?process_count:int -> string -> 'a
val spfoldr1 : (char -> char -> char) -> ?process_count:int -> string -> char
val spfoldlSeqN :
  ?process_count:int ->
  int -> ('a -> 'a -> 'a) -> ('a -> char -> 'a) -> 'a -> string -> 'a
val spiterSeqN :
  ?process_count:int ->
  int -> (char -> 'a) -> (char -> char) -> string -> unit
val ( ^* ) : int -> string -> string
val strip :
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  ?pat:string ->
  ?pos:int ->
  ?itempl:Pcre.substitution -> ?callout:Pcre.callout -> string -> string
val split : ?n:int -> string -> string -> string list
val rsplit : ?n:int -> string -> string -> string list
val nsplit : string -> int -> string -> string list
val nrsplit : string -> int -> string -> string list
val rx :
  ?study:bool ->
  ?limit:int ->
  ?iflags:Pcre.icflag ->
  ?flags:Pcre.cflag list -> ?chtables:Pcre.chtables -> string -> Pcre.regexp
val rex :
  ?study:bool ->
  ?limit:int ->
  ?iflags:Pcre.icflag ->
  ?flags:Pcre.cflag list -> ?chtables:Pcre.chtables -> string -> Pcre.regexp
val escape_rex : string -> string
val rexsplit : ?n:int -> Pcre.regexp -> string -> string list
val rexrsplit : ?n:int -> Pcre.regexp -> string -> string list
val xsplit : ?n:int -> string -> string -> string list
val xrsplit : ?n:int -> string -> string -> string list
val xnsplit : string -> int -> string -> string list
val xnrsplit : string -> int -> string -> string list
val rexscan : Pcre.regexp -> string -> string list list
val scan : string -> string -> string list list
val rexscan_nth : Pcre.regexp -> int -> string -> string list
val scan_nth : string -> int -> string -> string list
val xfind : string -> string -> string
val xfindOpt : string -> string -> string option
val smatch :
  string ->
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  ?rex:Pcre.regexp -> ?pos:int -> ?callout:Pcre.callout -> string -> bool
val rexmatch :
  Pcre.regexp ->
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  ?pat:string -> ?pos:int -> ?callout:Pcre.callout -> string -> bool
val xmatch :
  string ->
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  ?pat:string -> ?pos:int -> ?callout:Pcre.callout -> string -> bool
val replace :
  string ->
  string ->
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  ?rex:Pcre.regexp ->
  ?pos:int ->
  ?itempl:Pcre.substitution -> ?callout:Pcre.callout -> string -> string
val rexreplace :
  Pcre.regexp ->
  string ->
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  ?pat:string ->
  ?pos:int ->
  ?itempl:Pcre.substitution -> ?callout:Pcre.callout -> string -> string
val xreplace :
  string ->
  string ->
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  ?pat:string ->
  ?pos:int ->
  ?itempl:Pcre.substitution -> ?callout:Pcre.callout -> string -> string
val frexreplace : (string -> string) -> Pcre.regexp -> string -> string
val fxreplace : (string -> string) -> string -> string -> string
val quote : string -> string -> string -> string
val join : string -> string list -> string
val join_array : string -> string array -> string
val xreplaceMulti : (string * string) list -> string -> string
val replaceMulti : (string * string) list -> string -> string
val words : string -> string list
val unwords : string list -> string
val lines : string -> string list
val unlines : string list -> string
val rexsplitPartition :
  Pcre.regexp -> string -> (string * string) list * string option
val xsplitPartition :
  string -> string -> (string * string) list * string option


val bmake : int -> int -> string
val bcreate : int -> string
val binit : (int -> int) -> int -> string
val blen : string -> int
val bconcat : string list -> string
val breverse : 'a -> string -> string
val brev : 'a -> 'b array -> 'b array
val bmap : (int -> int) -> string -> string
val bmapSub : int -> int -> (int -> int) -> string -> string
val bmapSlice : int -> int -> (int -> int) -> string -> string
val bmapWithIndex : (int -> int -> int) -> string -> string
val biter : (int -> 'a) -> string -> unit
val biterSub : int -> int -> (int -> 'a) -> string -> unit
val biterSlice : int -> int -> (int -> 'a) -> string -> unit
val biterWithIndex : (int -> int -> 'a) -> string -> unit
val bfilter : (int -> bool) -> string -> string
val bfilterWithIndex : (int -> int -> bool) -> string -> string
val bfind : (int -> bool) -> string -> int
val bfindWithIndex : (int -> int -> bool) -> string -> int * int
val bfindIndex : (int -> bool) -> string -> int
val bfoldl : ('a -> int -> 'a) -> 'a -> string -> 'a
val bfoldl1 : (int -> int -> int) -> string -> int
val bfoldlSub : int -> int -> ('a -> int -> 'a) -> 'a -> string -> 'a
val bfoldl1Sub : int -> int -> (int -> int -> int) -> string -> int
val bfoldlSlice : int -> int -> ('a -> int -> 'a) -> 'a -> string -> 'a
val bfoldl1Slice : int -> int -> (int -> int -> int) -> string -> int
val bfoldr : (int -> 'a -> 'a) -> 'a -> string -> 'a
val bfoldr1 : (int -> int -> int) -> string -> int
val bfoldrSub : int -> int -> ('a -> int -> 'a) -> 'a -> string -> 'a
val bfoldr1Sub : int -> int -> (int -> int -> int) -> string -> int
val bfoldrSlice : int -> int -> ('a -> int -> 'a) -> 'a -> string -> 'a
val bfoldr1Slice : int -> int -> (int -> int -> int) -> string -> int
val bsum : string -> int
val bsumSub : int -> int -> string -> int
val bsumSlice : int -> int -> string -> int
val bsumf : string -> float
val bsumSubf : int -> int -> string -> float
val bsumSlicef : int -> int -> string -> float
val bproduct : string -> int
val bproductSub : int -> int -> string -> int
val bproductSlice : int -> int -> string -> int
val bproductf : string -> float
val bproductSubf : int -> int -> string -> float
val bproductSlicef : int -> int -> string -> float
val baverage : string -> int
val baverageSub : int -> int -> string -> int
val baverageSlice : int -> int -> string -> int
val baveragef : string -> float
val baverageSubf : int -> int -> string -> float
val baverageSlicef : int -> int -> string -> float
val bzipWith : (int -> int -> int) -> string -> string -> string
val bmap2 : (int -> int -> int) -> string -> string -> string
val bzipWith3 :
  (int -> int -> int -> int) -> string -> string -> string -> string
val bmap3 :
  (int -> int -> int -> int) -> string -> string -> string -> string
val bsub : int -> int -> string -> string
val bslice : int -> int -> string -> string
val bsubStride : int -> int -> int -> string -> string
val bgroupsOf : int -> string -> string list
val bsplitInto : int -> string -> string list
val btimes : int -> string -> string
val bpick : int list -> string -> int list
val bpickWith : ('a -> 'b) list -> 'a -> 'b list
val bpmap : (int -> int) -> ?process_count:int -> string -> string
val bpiter : (int -> 'a) -> ?process_count:int -> string -> unit
val bpinit : ?process_count:int -> (int -> int) -> int -> string
val bpzipWith :
  ?process_count:int -> (int -> int -> int) -> string -> string -> string
val bpfilter : (int -> bool) -> ?process_count:int -> string -> string
val bpfoldl :
  ('a -> 'a -> 'a) ->
  ('a -> int -> 'a) -> 'a -> ?process_count:int -> string -> 'a
val bpfoldl1 : (int -> int -> int) -> ?process_count:int -> string -> int
val bpfoldr :
  ('a -> 'a -> 'a) ->
  (int -> 'a -> 'a) -> 'a -> ?process_count:int -> string -> 'a
val bpfoldr1 : (int -> int -> int) -> ?process_count:int -> string -> int
val bpfoldlSeqN :
  ?process_count:int ->
  int -> ('a -> 'a -> 'a) -> ('a -> int -> 'a) -> 'a -> string -> 'a
val bpiterSeqN :
  ?process_count:int -> int -> (int -> 'a) -> (int -> int) -> string -> unit


val rename : string -> string -> unit
val ls : string -> string list
val rm : string -> unit
val ln_s : string -> string -> unit
val ln : string -> string -> unit
val mkdir : ?perm:Unix.file_perm -> string -> unit
val rmdir : string -> unit
val getcwd : unit -> string
val pwd : unit -> string
val chdir : string -> unit
val cd : string -> unit
val chmod : Unix.file_perm -> string -> unit
val chownUid : ?gid:int -> int -> string -> unit
val chown : ?group:string -> string -> string -> unit
val chgrpGid : int -> string -> unit
val chgrp : string -> string -> unit


val expandPath : string -> string
module Path :
  sig
    type t = Path of string list
    val absolute : string list -> string list
    val make : string -> t
    val to_s : t -> string
    val join_path : t -> t -> t
    val join_list : t -> string list -> t
    val join : t -> string -> t
    val join_list_to_s : t -> string list -> string
    val join_to_s : t -> string -> string
    val expand : t -> t
  end
val ( ^/ ) : string -> string -> string
val dirExists : string -> bool
val isRoot : string -> bool
val parentDirs : string -> string list
val dirSeparator : string
val splitPath : string -> string list
val joinPath : string list -> string
val relativePath : string -> string
val dirname : string -> string
val basename : string -> string
val mkdir_p : ?perm:Unix.file_perm -> string -> unit


val putStr : string -> unit
val putStrLn : string -> unit
val puts : string -> unit
val output_line : out_channel -> string -> unit
val readLine : in_channel -> string
val readChar : in_channel -> char
val readByte : in_channel -> int
val readInt : in_channel -> int
val readFloat : in_channel -> float
val open_append : string -> out_channel
val open_append_bin : string -> out_channel
val fileExists : string -> bool
val withFile : string -> (in_channel -> 'a) -> 'a
val withFileOut : string -> (out_channel -> 'a) -> 'a
val withFileAppend : string -> (out_channel -> 'a) -> 'a
val withUnixFile :
  ?flags:Unix.open_flag list ->
  ?perm:Unix.file_perm -> string -> (Unix.file_descr -> 'a) -> 'a
val withUnixFileOut :
  ?flags:Unix.open_flag list ->
  ?perm:Unix.file_perm -> string -> (Unix.file_descr -> 'a) -> 'a
val withUnixFileAppend :
  ?flags:Unix.open_flag list ->
  ?perm:Unix.file_perm -> string -> (Unix.file_descr -> 'a) -> 'a
val read : ?buf:string -> int -> in_channel -> string
val write : out_channel -> string -> unit
val readAll : in_channel -> string
val stat : string -> Unix.stats
val fileSize : string -> int
val fileKind : string -> Unix.file_kind
val isKind : Unix.file_kind -> string -> bool
val isDir : string -> bool
val isFile : string -> bool
val isLink : string -> bool
val isFIFO : string -> bool
val isSocket : string -> bool
val isCharDev : string -> bool
val isBlockDev : string -> bool
val fileInode : string -> int
val filePermissions : string -> Unix.file_perm
val fileDevice : string -> int
val fileUid : string -> int
val fileOwner : string -> string
val fileGid : string -> int
val fileGroup : string -> string
val atime : string -> float
val mtime : string -> float
val ctime : string -> float
val readFile : string -> string
val writeFile : string -> string -> unit
val appendFile : string -> string -> unit
val readLines : string -> string list
val tokenize : ('a -> 'b) -> 'a -> 'b list
val tokenizeN : ('a -> 'b) -> int -> 'a -> 'b list
val tokenizeIter : ('a -> 'b) -> ('b -> 'c) -> 'a -> unit
val tokenizeMap : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c list
val tokenizeFile : (in_channel -> 'a) -> string -> 'a list
val tokenizeFileN : (in_channel -> 'a) -> int -> string -> 'a list
val icEachLine : (string -> 'a) -> in_channel -> unit
val icMapLines : (string -> 'a) -> in_channel -> 'a list
val eachLine : (string -> 'a) -> string -> unit
val mapLines : (string -> 'a) -> string -> 'a list
val output_line_flush : out_channel -> string -> unit
val withTempFile : string -> (string -> 'a) -> 'a
val pipeWith : ('a -> 'b -> 'c -> 'c option) -> 'c -> 'a -> 'b -> 'c
val pipeChan : ('a -> 'b -> 'c -> 'c) -> 'c -> 'a -> 'b -> 'c
val unitPipe : (('a -> unit -> 'b * unit) -> 'c) -> ('a -> 'b) -> 'c
val pipeTokenizer :
  ('a -> 'b) ->
  ('c -> 'd -> 'e) -> ('b -> 'f -> 'd * 'g) -> 'a -> 'c -> 'f -> 'g
val linePiper :
  (string -> unit -> string * unit) ->
  in_channel -> out_channel -> unit -> unit
val blockPiper :
  ?buf:string ->
  int ->
  (string -> 'a -> string * 'b) -> in_channel -> out_channel -> 'a -> 'b
val pipeLines :
  (string -> unit -> string * unit) ->
  unit -> in_channel -> out_channel -> unit
val pipeBlocks :
  int ->
  (string -> 'a -> string * 'a) -> 'a -> in_channel -> out_channel -> 'a
val withFiles : (in_channel -> out_channel -> 'a) -> string -> string -> 'a
val withFilesAppend :
  (in_channel -> out_channel -> 'a) -> string -> string -> 'a
val pipeFiles :
  (in_channel -> out_channel -> 'a -> 'a) -> 'a -> string -> string -> 'a
val pipeFileLines :
  (string -> unit -> string * unit) -> unit -> string -> string -> unit
val pipeFileBlocks :
  int -> (string -> 'a -> string * 'a) -> 'a -> string -> string -> 'a
val pipeAppend :
  (in_channel -> out_channel -> 'a -> 'a) -> 'a -> string -> string -> 'a
val pipeAppendLines :
  (string -> unit -> string * unit) -> unit -> string -> string -> unit
val pipeAppendBlocks :
  int -> (string -> 'a -> string * 'a) -> 'a -> string -> string -> 'a
val interactWith : (string -> string) -> in_channel -> out_channel -> unit
val interact : (string -> string) -> unit
val interactFiles : (string -> string) -> string -> string -> unit
val interactAppend : (string -> string) -> string -> string -> unit
val appendFileTo : out_channel -> string -> unit
val cp : string -> string -> unit
val mv : string -> string -> unit
val prependFile : string -> string -> unit


val shell_escape :
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  ?pat:string ->
  ?pos:int ->
  ?itempl:Pcre.substitution -> ?callout:Pcre.callout -> string -> string
val escape_cmd : string list -> string
exception Command_error of int * string
val command : string list -> unit
val runCmd : string list -> unit
val cmdCode : string list -> int
val withRawCmd : string -> (in_channel -> out_channel -> 'a) -> 'a
val withRawCmdStdin : string -> (out_channel -> 'a) -> 'a
val withRawCmdStdout : string -> (in_channel -> 'a) -> 'a
val withCmd : string list -> (in_channel -> out_channel -> 'a) -> 'a
val withCmdStdin : string list -> (out_channel -> 'a) -> 'a
val withCmdStdout : string list -> (in_channel -> 'a) -> 'a
val readCmd : string list -> string
val readRawCmd : string -> string
val pipeCmd :
  (in_channel -> out_channel -> 'a -> 'a) -> 'a -> string list -> 'a
val pipeCmdLines :
  (string -> unit -> string * unit) -> unit -> string list -> unit
val pipeRawCmd :
  (in_channel -> out_channel -> 'a -> 'a) -> 'a -> string -> 'a
val pipeRawCmdLines :
  (string -> unit -> string * unit) -> unit -> string -> unit
val interactWithRawCmd : (string -> string) -> string -> unit
val interactWithCmd : (string -> string) -> string list -> unit


val bacreate :
  ?layout:Bigarray.c_layout Bigarray.layout ->
  ('a, 'b) Bigarray.kind ->
  int -> ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
val bafill : ('a, 'b, 'c) Bigarray.Array1.t -> 'a -> unit
val basub :
  ('a, 'b, 'c) Bigarray.Array1.t ->
  int -> int -> ('a, 'b, 'c) Bigarray.Array1.t
val bablit :
  ('a, 'b, 'c) Bigarray.Array1.t -> ('a, 'b, 'c) Bigarray.Array1.t -> unit
val bamake :
  ?layout:Bigarray.c_layout Bigarray.layout ->
  ('a, 'b) Bigarray.kind ->
  int -> 'a -> ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
val baget : ('a, 'b, 'c) Bigarray.Array1.t -> int -> 'a
val baset : ('a, 'b, 'c) Bigarray.Array1.t -> int -> 'a -> unit
val balayout : ('a, 'b, 'c) Bigarray.Array1.t -> 'c Bigarray.layout
val bakind : ('a, 'b, 'c) Bigarray.Array1.t -> ('a, 'b) Bigarray.kind
val balen : ('a, 'b, 'c) Bigarray.Array1.t -> int
val bainit :
  ?layout:Bigarray.c_layout Bigarray.layout ->
  ('a, 'b) Bigarray.kind ->
  (int -> 'a) -> int -> ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
val baiter : ('a -> 'b) -> ('a, 'c, 'd) Bigarray.Array1.t -> unit
val of_string :
  string ->
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
val to_string : (char, 'a, 'b) Bigarray.Array1.t -> string
val bamap :
  ('a -> 'a) ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
val bamapi :
  (int -> 'a -> 'a) ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
val bamapWithIndex :
  ('a -> int -> 'a) ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
val bafoldl : ('a -> 'b -> 'a) -> 'a -> ('b, 'c, 'd) Bigarray.Array1.t -> 'a
val bafoldr : ('a -> 'b -> 'b) -> 'b -> ('a, 'c, 'd) Bigarray.Array1.t -> 'b
val barev :
  ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
val baZipWith :
  ('a -> 'b -> 'a) ->
  ('a, 'c, Bigarray.c_layout) Bigarray.Array1.t ->
  ('b, 'd, 'e) Bigarray.Array1.t ->
  ('a, 'c, Bigarray.c_layout) Bigarray.Array1.t
val bamap2 :
  ('a -> 'b -> 'a) ->
  ('a, 'c, Bigarray.c_layout) Bigarray.Array1.t ->
  ('b, 'd, 'e) Bigarray.Array1.t ->
  ('a, 'c, Bigarray.c_layout) Bigarray.Array1.t
val bacreateMmap :
  ?layout:Bigarray.c_layout Bigarray.layout ->
  ?shared:bool ->
  ?perm:Unix.file_perm ->
  ?flags:Unix.open_flag list ->
  ('a, 'b) Bigarray.kind ->
  int -> string -> ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
val bacreateShared :
  ?layout:Bigarray.c_layout Bigarray.layout ->
  ('a, 'b) Bigarray.kind ->
  int -> ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
