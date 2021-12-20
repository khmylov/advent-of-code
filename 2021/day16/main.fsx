#nowarn "25"
#nowarn "40"

open System

let input = "E054831006016008CF01CED7CDB2D495A473336CF7B8C8318021C00FACFD3125B9FA624BD3DBB7968C0179DFDBD196FAE5400974A974B55C24DC580085925D5007E2D49C6579E49252E28600B580272379054AF57A54D65E1586A951D860400434E36080410926624D25458890A006CA251006573D2DFCBF4016919CC0A467302100565CF24B7A9C36B0402840002150CA3E46000042621C108F0200CC5C8551EA47F79FC28401C20042E0EC288D4600F42585F1F88010C8C709235180272B3DCAD95DC005F6671379988A1380372D8FF1127BDC0D834600BC9334EA5880333E7F3C6B2FBE1B98025600A8803F04E2E45700043E34C5F8A72DDC6B7E8E400C01797D02D002052637263CE016CE5E5C8CC9E4B369E7051304F3509627A907C97BCF66008500521395A62553A9CAD312A9CCCEAF63A500A2631CCD8065681D2479371E4A90E024AD69AAEBE20002A84ACA51EE0365B74A6BF4B2CC178153399F3BACC68CF3F50840095A33CBD7EF1393459E2C3004340109596AB6DEBF9A95CACB55B6F5FCD4A24580400A8586009C70C00D44401D8AB11A210002190DE1BC43872C006C45299463005EC0169AFFF6F9273269B89F4F80100507C00A84EB34B5F2772CB122D26016CA88C9BCC8BD4A05CA2CCABF90030534D3226B32D040147F802537B888CD59265C3CC01498A6B7BA7A1A08F005C401C86B10A358803D1FE24419300524F32AD2C6DA009080330DE2941B1006618450822A009C68998C1E0C017C0041A450A554A582D8034797FD73D4396C1848FC0A6F14503004340169D96BE1B11674A4804CD9DC26D006E20008747585D0AC001088550560F9019B0E004080160058798012804E4801232C0437B00F70A005100CFEE007A8010C02553007FC801A5100530C00F4B0027EE004CA64A480287C005E27EEE13DD83447D3009E754E29CDB5CD3C"

type PacketData = | Literal of int64 | Operator of Packet list
and Packet = Packet of version: int * typeId: int * children: PacketData

let toDecimal (input: int seq) =
  let mutable result = 0L
  for bit in input do
    result <- result <<< 1
    result <- result + int64 bit
  result

type 'a Parser = int list -> 'a * int list

let parseBits bitLength transform input =
  let bits, tail = List.splitAt bitLength input
  transform bits, tail

let parseBitsAsNumber bitLength = parseBits bitLength (toDecimal >> int)

let compose first second transform input =
  let a, tail1 = first input
  let b, tail2 = second tail1
  transform a b, tail2

let dispatch parseDiscriminator parseResult input =
  let value, tail1 = parseDiscriminator input
  let nextParser = parseResult value
  nextParser tail1

let inline mapFst fn (a, b) = fn a, b

let inline mapParser fn parser = parser >> mapFst fn

let parseLiteralPacket =
  let rec loop bits =
    let prefix::group, tail = List.splitAt 5 bits
    if prefix = 0 then [], tail else loop tail
    |> mapFst (fun next -> group@next)
  loop >> mapFst (toDecimal >> Literal)

let rec parseSubPackets subPacketBitLength input =
  let rec loop acc remainInput =
    if List.length input - List.length remainInput >= subPacketBitLength then acc, remainInput
    else let parsed, tail = parsePacket remainInput in loop (parsed::acc) tail
  loop [] input

and parseSubPacketsByCount count input =
  seq {1..count}
  |> Seq.fold (fun (acc, remainInput) _ -> remainInput |> parsePacket |> mapFst (fun a -> a::acc)) ([], input)

and parseOperatorPacket =
  dispatch
    (parseBitsAsNumber 1)
    (function
    | 0 -> dispatch (parseBitsAsNumber 15) parseSubPackets
    | 1 -> dispatch (parseBitsAsNumber 11) parseSubPacketsByCount)
  |> mapParser Operator

and parsePacket =
  compose
    (parseBitsAsNumber 3) // version
    (dispatch
      (parseBitsAsNumber 3) // type
      (fun typeId ->
        match typeId with | 4 -> parseLiteralPacket | _ -> parseOperatorPacket
        |> mapParser (fun res -> typeId, res)
      )
    )
    (fun version (typeId, inner) -> Packet (version, typeId, inner))

let parsed, _ =
  input
  |> Seq.map (fun c -> Convert.ToInt32(c.ToString(), 16))
  |> Seq.collect (fun c -> Convert.ToString(c, 2).PadLeft(4, '0'))
  |> Seq.map (fun x -> int x - int '0')
  |> List.ofSeq
  |> parsePacket

// 875
let part1 =
  let rec sum (Packet (version, _, data)) =
    match data with | Literal _ -> version | Operator ps -> version + List.sumBy sum ps

  sum parsed

// 1264857437203
let part2 =
  let rec eval (Packet (_, typeId, data)) =
    match typeId with
    | 0 -> data |> evalChildren |> List.sum
    | 1 -> data |> evalChildren |> List.fold (*) 1L
    | 2 -> data |> evalChildren |> List.min
    | 3 -> data |> evalChildren |> List.max
    | 4 -> let (Literal x) = data in x
    | 5 -> let (Operator [p2; p1]) = data in if eval p1 > eval p2 then 1L else 0L
    | 6 -> let (Operator [p2; p1]) = data in if eval p1 < eval p2 then 1L else 0L
    | 7 -> let (Operator [p2; p1]) = data in if eval p1 = eval p2 then 1L else 0L
  and evalChildren = function | Literal x -> [x] | Operator ps -> List.map eval ps

  eval parsed
