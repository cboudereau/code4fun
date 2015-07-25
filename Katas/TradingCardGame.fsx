#r "System.Speech"

open System.Speech.Synthesis
let player1Voice = new SpeechSynthesizer();
player1Voice.GetInstalledVoices() |> Seq.map(fun v -> v.VoiceInfo.Name,v.VoiceInfo.Age, v.VoiceInfo.Culture.IetfLanguageTag, v.VoiceInfo.Gender)
player1Voice.SelectVoice("Microsoft David Desktop")

let player2Voice = new SpeechSynthesizer();
player2Voice.SelectVoice("Microsoft Hazel Desktop")

let arbiter = new SpeechSynthesizer();
arbiter.SelectVoice("Microsoft Zira Desktop")

type Mana = Mana of int

type Slot = Slot

type Capacity = 
    | Capacity of int
    static member add = function
        | Capacity c when c >= 10 -> Capacity c
        | Capacity c -> c + 1 |> Capacity

type Slots = 
    | Slots of Capacity * Slot list
    static member add slots = 
        match slots with
        | Slots (capacity, slots) when slots |> List.length >= 10 -> Slots (capacity, slots)
        | Slots (capacity, slots) -> Slots (capacity |> Capacity.add, Slot::slots)
    static member reload = function
        Slots (Capacity c, _) -> 
            printfn "reloaded with capacity %i" c
            Slots (Capacity c, [1..c] |> List.map(fun _-> Slot))
    static member count = function Slots (_, slots) -> slots |> List.length        
    static member empty = Slots (Capacity 0,[])

type Card = Card of Mana

type Deck = 
    | Deck of Card list
    static member draw deck = 
        match deck with
        | Deck cards -> 
            match cards with
            | [] -> None
            | card :: deck -> Some (card, deck)

type Life = 
    | Life of int
    static member attack cards life =
        match life with
        | Life point -> 
            let damage = 
                cards 
                |> List.map(function Card (Mana d) -> d)
                |> List.sum
            Life (point - damage)
    static member hurt = function Life l -> l - 1 |> Life

type Hand = 
    | Hand of Card list
    static member add card hand = 
        match hand with
        | Hand cards -> Hand (card :: cards)
    static member take hand = 
        match hand with
        | Hand (card :: cards) -> (Some card, Hand cards)
        | Hand [] -> (None, Hand [])
    static member highest hand =
        match hand with
        | Hand [] -> (None, Hand [])
        | Hand cards -> Some (cards |> List.maxBy(function Card(Mana p)->p)), Hand cards.Tail
    static member empty = Hand []

type Player = 
    { name: string
      slots: Slots
      deck: Deck
      hand: Hand
      life: Life
      speaker: string -> unit
      strategy: Player -> Card list * Player }
    member this.speak text = text |> this.speaker

type EndGame = 
    { winner: Player
      looser: Player
      pass: int }

type Players = Players of Player * Player

type Game = 
    { turn: Players
      pass: int }

type GameState = 
    | Started of Game
    | End of EndGame

let draw p = 
    match p.deck |> Deck.draw with
    | None -> { p with life = p.life |> Life.hurt }
    | Some (card, deck) -> 
        { p with 
            hand = p.hand |> Hand.add card
            deck = Deck deck } 

let newPlayer name speaker strategy deck = 
    let player = 
        { name = name
          slots = Slots.empty 
          deck = deck
          hand = Hand.empty
          life = Life 30
          speaker = speaker
          strategy = strategy }
    let readyPlayer = player |> draw |> draw |> draw

    readyPlayer.speak (sprintf "Hello I'm %s and I will hurt you!!!!!" name) 
    readyPlayer

let rnd = System.Random()
let random _ = rnd.Next()

let newDeck random = 
    [ 0;0;1;1;2;2;2;3;3;3;3;4;4;4;5;5;6;6;7;8 ] 
    |> List.map(fun i -> i |> Mana |> Card)
    |> List.sortBy random
    |> Deck

let chooseOrder (s1,p1) (s2,p2) = 
    let arbiterIntroduce player = arbiter.Speak(sprintf "Player %s is the first player" player)
    
    if (random p1 % 2 = 0) 
    then 
        arbiterIntroduce p1
        ((s1,p1), (s2,p2)) 
    else 
        arbiterIntroduce p2
        ((s2,p2),(s1,p1))

let startGame person1 person2 = 
    let ((s1,p1),(s2,p2)) = chooseOrder person1 person2
    
    let player1 = random |> newDeck |> newPlayer p1 player1Voice.Speak s1
    let player2 = random |> newDeck |> newPlayer p2 player2Voice.Speak s2 |> draw
    
    Started { turn= Players (player1, player2); pass=0 } 

let hurt cards p2 = 
    match p2.life |> Life.attack cards with
    | Life point -> 
        printfn "player %s with %A, %i slots and cards %A" p2.name p2.life (p2.slots |> Slots.count) cards
        { p2 with life = Life point }

let kamikaze p = 
    let slot = p.slots |> Slots.count

    let (cards, p) = 
        let rec internalCards remaining player cards = 
            match remaining, player.hand |> Hand.highest with
            | 0, _ -> cards, player
            | remaining, (Some card, hand) -> internalCards (remaining - 1) { player with hand=hand } (card::cards)
            | remaining, (None, hand) -> internalCards (remaining - 1) { player with hand=hand; life=player.life |> Life.hurt  } cards
        internalCards slot p []
    
    let mana = cards |> Seq.map(function Card (Mana i) -> i) |> Seq.sum
    p.speak (sprintf "I hurt you with %i mana!" mana)

    cards, p

let addSlot p = { p with slots = p.slots |> Slots.add }

let turn game = 
    let (p1,p2) = match game.turn with Players (ps1,ps2) -> (ps1,ps2)
    
    let p1 = p1 |> draw |> addSlot
    printfn "player %s has %i slot" p1.name (p1.slots |> Slots.count)
    let (cards, p1) = p1 |> p1.strategy
    printfn "player %s choose cards %A" p1.name cards
    let p2 = p2 |> hurt cards
    (p2,p1)

let (|Dead|_|) life = 
    match life with
    | Life l when l <= 0 -> Some l
    | Life l -> None

let rec play game = 
    match game with
    | End p -> End p
    | Started game ->
        let (p1, p2) = game |> turn

        match p1.life, p2.life with
        | Dead _, _  -> End { winner=p2; looser=p1; pass=game.pass }
        | _, Dead _ -> End { winner=p1; looser=p2; pass=game.pass }
        | _, _ -> 
            { turn = Players ({p1 with slots=p1.slots |> Slots.reload}, {p2 with slots=p2.slots |> Slots.reload}); pass= game.pass + 1 }
            |> Started
            |> play

let game = (kamikaze, "Crazy") |> startGame (kamikaze, "Drunk")

let endGame = play game

match endGame with
| End g -> 
    let message = sprintf "\n\nGame Over in %i turn! \nThe winner is %s\nYou loose %s !" g.pass g.looser.name g.winner.name
    arbiter.Speak message
| _ -> printfn "started"

