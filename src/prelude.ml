(*
Prelude.ml: OCaml utility functions

Copyright (C) 2007-2008  Ilmari Heikkinen <ilmari.heikkinen@gmail.com>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

*)

include Printf

include PreCombinators
include PreFilesystem
include PrePath
include PreIo
include PreShell

include PreList
include PreString
include PreBytestring
include PreBigarray
include PreArray
include PreRange

include PreList.List

(* Array operation shortcuts *)

let amake = Array.make
let acreate = Array.create
let ainit = Array.init
let alen = Array.length
let aconcat = Array.concat
let areverse a = Array.reverse
let arev = areverse

let amap = Array.map
let amapSub = Array.mapSub
let amapSlice = Array.mapSlice
let amapWithIndex = Array.mapWithIndex
let aiter = Array.iter
let aiterSub = Array.iterSub
let aiterSlice = Array.iterSlice
let aiterWithIndex = Array.iterWithIndex
let afilter = Array.filter
let afilterWithIndex = Array.filterWithIndex
let afind = Array.find
let afindWithIndex = Array.findWithIndex
let afindIndex = Array.findIndex

let afoldl = Array.foldl
let afoldl1 = Array.foldl1
let afoldlSub = Array.foldlSub
let afoldl1Sub = Array.foldl1Sub
let afoldlSlice = Array.foldlSlice
let afoldl1Slice = Array.foldl1Slice

let afoldr = Array.foldr
let afoldr1 = Array.foldr1
let afoldrSub = Array.foldrSub
let afoldr1Sub = Array.foldr1Sub
let afoldrSlice = Array.foldrSlice
let afoldr1Slice = Array.foldr1Slice

let asum = Array.sum
let asumSub = Array.sumSub
let asumSlice = Array.sumSlice

let asumf = Array.sumf
let asumSubf = Array.sumSubf
let asumSlicef = Array.sumSlicef

let aproduct = Array.product
let aproductSub = Array.productSub
let aproductSlice = Array.productSlice

let aproductf = Array.productf
let aproductSubf = Array.productSubf
let aproductSlicef = Array.productSlicef

let aaverage = Array.average
let aaverageSub = Array.averageSub
let aaverageSlice = Array.averageSlice

let aaveragef = Array.averagef
let aaverageSubf = Array.averageSubf
let aaverageSlicef = Array.averageSlicef

let arange = Array.range
let azipWith = Array.zipWith
let amap2 = Array.zipWith
let azipWith3 = Array.zipWith3
let amap3 = Array.zipWith3
let asub = Array.sub
let aslice = Array.slice
let asubStride = Array.subStride
let agroupsOf = Array.groupsOf
let asplitInto = Array.splitInto
let atimes = Array.times

let apick = Array.pick
let apickWith = Array.pickWith

let apmap = Array.pmap
let apiter = Array.piter
let apinit = Array.pinit
let apzipWith = Array.pzipWith
let apfilter = Array.pfilter
let apfoldl = Array.pfoldl
let apfoldl1 = Array.pfoldl1
let apfoldr = Array.pfoldr
let apfoldr1 = Array.pfoldr1
let apfoldlSeqN = Array.pfoldlSeqN
let apiterSeqN = Array.piterSeqN

let (@|) = Array.append
let (@|*) = Array.times
let (--|) = Array.range


(* Bigarray operation shortcuts *)
(*
let bamake = Bigarray.make
let bacreate = Bigarray.create
let bainit = Bigarray.init
let balen = Bigarray.length
let baconcat = Bigarray.concat
let bareverse a = Bigarray.reverse
let barev = areverse

let bamap = Bigarray.map
let bamapSub = Bigarray.mapSub
let bamapSlice = Bigarray.mapSlice
let bamapWithIndex = Bigarray.mapWithIndex
let baiter = Bigarray.iter
let baiterSub = Bigarray.iterSub
let baiterSlice = Bigarray.iterSlice
let baiterWithIndex = Bigarray.iterWithIndex
let bafilter = Bigarray.filter
let bafilterWithIndex = Bigarray.filterWithIndex
let bafind = Bigarray.find
let bafindWithIndex = Bigarray.findWithIndex
let bafindIndex = Bigarray.findIndex

let bafoldl = Bigarray.foldl
let bafoldl1 = Bigarray.foldl1
let bafoldlSub = Bigarray.foldlSub
let bafoldl1Sub = Bigarray.foldl1Sub
let bafoldlSlice = Bigarray.foldlSlice
let bafoldl1Slice = Bigarray.foldl1Slice

let bafoldr = Bigarray.foldr
let bafoldr1 = Bigarray.foldr1
let bafoldrSub = Bigarray.foldrSub
let bafoldr1Sub = Bigarray.foldr1Sub
let bafoldrSlice = Bigarray.foldrSlice
let bafoldr1Slice = Bigarray.foldr1Slice

let basum = Bigarray.sum
let basumSub = Bigarray.sumSub
let basumSlice = Bigarray.sumSlice

let basumf = Bigarray.sumf
let basumSubf = Bigarray.sumSubf
let basumSlicef = Bigarray.sumSlicef

let baproduct = Bigarray.product
let baproductSub = Bigarray.productSub
let baproductSlice = Bigarray.productSlice

let baproductf = Bigarray.productf
let baproductSubf = Bigarray.productSubf
let baproductSlicef = Bigarray.productSlicef

let baaverage = Bigarray.average
let baaverageSub = Bigarray.averageSub
let baaverageSlice = Bigarray.averageSlice

let baaveragef = Bigarray.averagef
let baaverageSubf = Bigarray.averageSubf
let baaverageSlicef = Bigarray.averageSlicef

let barange = Bigarray.range
let bazipWith = Bigarray.zipWith
let bamap2 = Bigarray.zipWith
let bazipWith3 = Bigarray.zipWith3
let bamap3 = Bigarray.zipWith3
let basub = Bigarray.sub
let baslice = Bigarray.slice
let basubStride = Bigarray.subStride
let bagroupsOf = Bigarray.groupsOf
let basplitInto = Bigarray.splitInto
let batimes = Bigarray.times

let bapick = Bigarray.pick
let bapickWith = Bigarray.pickWith

let bapmap = Bigarray.pmap
let bapiter = Bigarray.piter
let bapinit = Bigarray.pinit
let bapzipWith = Bigarray.pzipWith
let bapfilter = Bigarray.pfilter
let bapfoldl = Bigarray.pfoldl
let bapfoldl1 = Bigarray.pfoldl1
let bapfoldr = Bigarray.pfoldr
let bapfoldr1 = Bigarray.pfoldr1
let bapfoldlSeqN = Bigarray.pfoldlSeqN
let bapiterSeqN = Bigarray.piterSeqN

let (@||) = Bigarray.append
let ( @||* ) = Bigarray.times
let (--||) = Bigarray.range
*)


(* String operation shortcuts *)

let smake = String.make
let screate = String.create
let sinit = String.init
let slen = String.length
let sconcat = String.concat
let sreverse a = String.reverse
let srev = areverse

let smap = String.map
let smapSub = String.mapSub
let smapSlice = String.mapSlice
let smapWithIndex = String.mapWithIndex
let siter = String.iter
let siterSub = String.iterSub
let siterSlice = String.iterSlice
let siterWithIndex = String.iterWithIndex
let sfilter = String.filter
let sfilterWithIndex = String.filterWithIndex
let sfind = String.find
let sfindWithIndex = String.findWithIndex
let sfindIndex = String.findIndex

let sfoldl = String.foldl
let sfoldl1 = String.foldl1
let sfoldlSub = String.foldlSub
let sfoldl1Sub = String.foldl1Sub
let sfoldlSlice = String.foldlSlice
let sfoldl1Slice = String.foldl1Slice

let sfoldr = String.foldr
let sfoldr1 = String.foldr1
let sfoldrSub = String.foldrSub
let sfoldr1Sub = String.foldr1Sub
let sfoldrSlice = String.foldrSlice
let sfoldr1Slice = String.foldr1Slice

let ssum = String.sum
let ssumSub = String.sumSub
let ssumSlice = String.sumSlice

let ssumf = String.sumf
let ssumSubf = String.sumSubf
let ssumSlicef = String.sumSlicef

let sproduct = String.product
let sproductSub = String.productSub
let sproductSlice = String.productSlice

let sproductf = String.productf
let sproductSubf = String.productSubf
let sproductSlicef = String.productSlicef

let saverage = String.average
let saverageSub = String.averageSub
let saverageSlice = String.averageSlice

let saveragef = String.averagef
let saverageSubf = String.averageSubf
let saverageSlicef = String.averageSlicef

(* let srange = String.range *)
let szipWith = String.zipWith
let smap2 = String.zipWith
let szipWith3 = String.zipWith3
let smap3 = String.zipWith3
let ssub = String.sub
let sslice = String.slice
let ssubStride = String.subStride
let sgroupsOf = String.groupsOf
let ssplitInto = String.splitInto
let stimes = String.times

let spick = String.pick
let spickWith = String.pickWith

let spmap = String.pmap
let spiter = String.piter
let spinit = String.pinit
let spzipWith = String.pzipWith
let spfilter = String.pfilter
let spfoldl = String.pfoldl
let spfoldl1 = String.pfoldl1
let spfoldr = String.pfoldr
let spfoldr1 = String.pfoldr1
let spfoldlSeqN = String.pfoldlSeqN
let spiterSeqN = String.piterSeqN

let (^*) = String.times


(* Bytestring operation shortcuts *)

let bmake = Bytestring.make
let bcreate = Bytestring.create
let binit = Bytestring.init
let blen = Bytestring.length
let bconcat = Bytestring.concat
let breverse a = Bytestring.reverse
let brev = areverse

let bmap = Bytestring.map
let bmapSub = Bytestring.mapSub
let bmapSlice = Bytestring.mapSlice
let bmapWithIndex = Bytestring.mapWithIndex
let biter = Bytestring.iter
let biterSub = Bytestring.iterSub
let biterSlice = Bytestring.iterSlice
let biterWithIndex = Bytestring.iterWithIndex
let bfilter = Bytestring.filter
let bfilterWithIndex = Bytestring.filterWithIndex
let bfind = Bytestring.find
let bfindWithIndex = Bytestring.findWithIndex
let bfindIndex = Bytestring.findIndex

let bfoldl = Bytestring.foldl
let bfoldl1 = Bytestring.foldl1
let bfoldlSub = Bytestring.foldlSub
let bfoldl1Sub = Bytestring.foldl1Sub
let bfoldlSlice = Bytestring.foldlSlice
let bfoldl1Slice = Bytestring.foldl1Slice

let bfoldr = Bytestring.foldr
let bfoldr1 = Bytestring.foldr1
let bfoldrSub = Bytestring.foldrSub
let bfoldr1Sub = Bytestring.foldr1Sub
let bfoldrSlice = Bytestring.foldrSlice
let bfoldr1Slice = Bytestring.foldr1Slice

let bsum = Bytestring.sum
let bsumSub = Bytestring.sumSub
let bsumSlice = Bytestring.sumSlice

let bsumf = Bytestring.sumf
let bsumSubf = Bytestring.sumSubf
let bsumSlicef = Bytestring.sumSlicef

let bproduct = Bytestring.product
let bproductSub = Bytestring.productSub
let bproductSlice = Bytestring.productSlice

let bproductf = Bytestring.productf
let bproductSubf = Bytestring.productSubf
let bproductSlicef = Bytestring.productSlicef

let baverage = Bytestring.average
let baverageSub = Bytestring.averageSub
let baverageSlice = Bytestring.averageSlice

let baveragef = Bytestring.averagef
let baverageSubf = Bytestring.averageSubf
let baverageSlicef = Bytestring.averageSlicef

(* let brange = Bytestring.range *)
let bzipWith = Bytestring.zipWith
let bmap2 = Bytestring.zipWith
let bzipWith3 = Bytestring.zipWith3
let bmap3 = Bytestring.zipWith3
let bsub = Bytestring.sub
let bslice = Bytestring.slice
let bsubStride = Bytestring.subStride
let bgroupsOf = Bytestring.groupsOf
let bsplitInto = Bytestring.splitInto
let btimes = Bytestring.times

let bpick = Bytestring.pick
let bpickWith = Bytestring.pickWith

let bpmap = Bytestring.pmap
let bpiter = Bytestring.piter
let bpinit = Bytestring.pinit
let bpzipWith = Bytestring.pzipWith
let bpfilter = Bytestring.pfilter
let bpfoldl = Bytestring.pfoldl
let bpfoldl1 = Bytestring.pfoldl1
let bpfoldr = Bytestring.pfoldr
let bpfoldr1 = Bytestring.pfoldr1
let bpfoldlSeqN = Bytestring.pfoldlSeqN
let bpiterSeqN = Bytestring.piterSeqN

