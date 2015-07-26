#r "System.Speech"

open System.Speech.Synthesis
let player1Voice = new SpeechSynthesizer();
player1Voice.GetInstalledVoices() |> Seq.map(fun v -> v.VoiceInfo.Name,v.VoiceInfo.Age, v.VoiceInfo.Culture.IetfLanguageTag, v.VoiceInfo.Gender)
player1Voice.SelectVoice("Microsoft Hortense Desktop")

let player2Voice = new SpeechSynthesizer();
player2Voice.SelectVoice("Microsoft Hortense Desktop")

let arbiter = new SpeechSynthesizer();
arbiter.SelectVoice("Microsoft Hortense Desktop")

let displaySpeech speaker text = 
    printfn "%s" text
//    speaker text
    async { 
//        do! Async.Sleep 500
        return () }

let asyncSpeak (s:SpeechSynthesizer) text = 
    let asyncCompleted (prompt:Prompt) = 
        async {
            let rec waitTrue (prompt:Prompt) = 
                async {
                    match prompt.IsCompleted with
                    | true -> return ()
                    | false -> 
                        do! Async.Sleep 10
                        return! waitTrue prompt
                }
            return! waitTrue prompt
        }
    
    async {
        let prompt = s.SpeakAsync(text:string)
        
        return! prompt |> asyncCompleted
    }

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
            (point - damage) |> Life.fromPoint
    static member hurt = function Life l -> l - 1 |> Life.fromPoint
    static member fromPoint v = match v with _ when v < 0 -> Life 0 | value -> Life value
    static member value = function Life l -> l

type Hand = 
    | Hand of Card list
    static member add card hand = 
        match hand with
        | Hand cards -> Hand (card :: cards)
    static member remove card hand = 
        match hand with
        | Hand hand when hand |> List.exists (fun c -> c=card) -> 
            let index = hand |> List.findIndex(fun c -> c=card)
            let hand = hand |> List.mapi(fun i c -> (i,c)) |> List.filter(fun (i,_)-> i<>index) |> List.map(fun (_,c) -> c)
            (Some card, hand)
        | Hand hand -> None, hand
    static member take hand = 
        match hand with
        | Hand (card :: cards) -> (Some card, Hand cards)
        | Hand [] -> (None, Hand [])
    static member highest hand =
        match hand with
        | Hand [] -> (None, Hand [])
        | Hand cards -> 
            let highestCards = cards |> List.sortBy(function Card(Mana p)-> - p)
            Some (highestCards.Head), Hand highestCards.Tail
    static member empty = Hand []
    static member isEmpty = function Hand hand -> match hand with [] -> true | _ -> false

type Player = 
    { name: string
      slots: Slots
      deck: Deck
      hand: Hand
      life: Life
      speak: string -> Async<unit>
      strategy: Player -> Card list * Player }

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
          speak = speaker
          strategy = strategy }
    let readyPlayer = player |> draw |> draw |> draw

//    readyPlayer.speaker (sprintf "Hello I'm %s and I will hurt you!!!!!" name) |> Async.RunSynchronously
    displaySpeech readyPlayer.speak (sprintf "Bonjour Je suis %s, Je vais te faire très mal!!!!!" name) |> Async.RunSynchronously
    readyPlayer

let rnd = System.Random()
let random _ = rnd.Next()

let newDeck random = 
    [ 0;0;1;1;2;2;2;3;3;3;3;4;4;4;5;5;6;6;7;8 ] 
    |> List.map(fun i -> i |> Mana |> Card)
    |> List.sortBy random
    |> Deck

let chooseOrder arbiter (s1,p1) (s2,p2) = 
//    let arbiterIntroduce player = arbiter.Speak(sprintf "Player %s is the first player" player)
    let arbiterIntroduce arbiter player = 
        displaySpeech arbiter (sprintf "%s est le premier joueur" player)
    
    if (random p1 % 2 = 0) 
    then 
        arbiterIntroduce arbiter p1 |> Async.RunSynchronously
        ((s1,p1), (s2,p2)) 
    else 
        arbiterIntroduce arbiter p2 |> Async.RunSynchronously
        ((s2,p2),(s1,p1))

let startGame arbiter tellName tellStrategy = 
    
    let p1 = tellName 1
    let s1 = tellStrategy 1

    let p2 = tellName 2
    let s2 = tellStrategy 2

    let ((s1,p1),(s2,p2)) = chooseOrder arbiter (s1,p1) (s2,p2)
    
    let player1 = random |> newDeck |> newPlayer p1 (player1Voice |> asyncSpeak) s1
    let player2 = random |> newDeck |> newPlayer p2 (player2Voice |> asyncSpeak) s2 |> draw
    
    Started { turn= Players (player1, player2); pass=0 } 

let hurt cards p2 = 
    match p2.life |> Life.attack cards with
    | Life point -> 
        let p2 = { p2 with life = Life point }
//        displaySpeech p2.speak (sprintf "I have been hurted. Now I have %i life" (p2.life |> Life.value)) |> Async.RunSynchronously
        displaySpeech p2.speak (sprintf "Je suis blessé. J'ai maintenant %i points de vie" (p2.life |> Life.value)) |> Async.RunSynchronously
        p2


let addSlot p = 
    let p = { p with slots = p.slots |> Slots.add }
//    displaySpeech p.speak (sprintf "%s win 1 slot and has now %i slots" p1.name (p1.slots |> Slots.count)) |> Async.RunSynchronously
    displaySpeech p.speak (sprintf "%s a gagné 1 slot et a maintenant %i slots" p.name (p.slots |> Slots.count)) |> Async.RunSynchronously
    p

let turn arbiter game = 
    let (p1,p2) = match game.turn with Players (p1,p2) -> (p1,p2)
//    displaySpeech arbiter (sprintf "Turn %i, score %s with %i life, %s with %i life" game.pass p1.name (p1.life |> Life.value) p2.name (p2.life |> Life.value))
    displaySpeech arbiter (sprintf "Tour %i. Le score est: %s : %i, %s : %i" game.pass p1.name (p1.life |> Life.value) p2.name (p2.life |> Life.value)) |> Async.RunSynchronously

    let p1 = p1 |> draw |> addSlot
    
    let (cards, p1) = p1 |> p1.strategy

    let p2 = p2 |> hurt cards

    (p2,p1)

let (|Dead|_|) life = 
    match life with
    | Life l when l <= 0 -> Some l
    | Life _ -> None

let rec play arbiter game = 
    match game with
    | End p -> End p
    | Started game ->
        let (p1, p2) = game |> turn arbiter

        match p1.life, p2.life with
        | Dead _, _  -> End { winner=p2; looser=p1; pass=game.pass }
        | _, Dead _ -> End { winner=p1; looser=p2; pass=game.pass }
        | _, _ -> 
            { turn = Players ({p1 with slots=p1.slots |> Slots.reload}, {p2 with slots=p2.slots |> Slots.reload}); pass= game.pass + 1 }
            |> Started
            |> play arbiter

let countMana cards = cards |> Seq.map(function Card (Mana i) -> i) |> Seq.sum

let kamikaze p = 
    let slot = p.slots |> Slots.count

    let (cards, p) = 
        let rec internalCards remaining player cards = 
            match remaining, player.hand |> Hand.highest with
            | 0, _ -> cards, player
            | remaining, (Some card, hand) -> internalCards (remaining - 1) { player with hand=hand } (card::cards)
            | remaining, (None, _) -> internalCards (remaining - 1) player cards
        internalCards slot p []
    
    let mana = countMana cards
    
//    displaySpeech p.speaker (sprintf "I have choosen %A" cards) |> Async.RunSynchronously
    displaySpeech p.speak (sprintf "%s a choisi %A" p.name cards) |> Async.RunSynchronously
//    displaySpeech p.speaker (sprintf "I hurt you with %i mana!" mana) |> Async.RunSynchronously
    displaySpeech p.speak (sprintf "%s a attaqué avec %i point de mana!" p.name mana) |> Async.RunSynchronously

    cards, p

let human p =
    let slot = p.slots |> Slots.count
    
    let rec askOneCard p = 
        
        if(p.hand |> Hand.isEmpty) 
        then (None, p)
        else
        
//            displaySpeech p.speaker (sprintf "Choose a card of mana : %A" p.hand) |> Async.Start
            displaySpeech p.speak (sprintf "%s, choisi l'une de ces cartes : %A" p.name p.hand) |> Async.Start
            let value = System.Console.ReadLine()

            match System.Int32.TryParse(value) with
            | (false, _) -> 
//                displaySpeech p.speaker (sprintf "This is not a valid card.") |> Async.RunSynchronously
                displaySpeech p.speak (sprintf "%s, cette carte n'est pas valide" p.name) |> Async.RunSynchronously
                p |> askOneCard
            | (true, mana) -> 
                let card = mana |> Mana |> Card
                match p.hand |> Hand.remove card with
                | None, hand when hand |> List.isEmpty -> None, { p with hand=Hand hand }
                | None, _ -> 
//                    displaySpeech p.speaker (sprintf "Bad card, given a valid point of mana.") |> Async.RunSynchronously
                    displaySpeech p.speak (sprintf "%s, mauvaise carte!" p.name) |> Async.RunSynchronously
                    askOneCard p
                | Some card, hand -> 
                    Some card, { p with hand=Hand hand }
    
    let rec askCards remaining player cards =
        match remaining with
        | 0 -> cards, player
        | _ -> 
            match player |> askOneCard with
            | None, p -> cards, p
            | Some card, p -> askCards (remaining - 1) p (card::cards)

    let (cards,p) = askCards slot p []
    let mana = countMana cards
    
//    displaySpeech p.speaker (sprintf "I have choosen %A and i hurt you with %i" cards mana) |> Async.RunSynchronously
    displaySpeech p.speak (sprintf "J'ai choisi %A et je t'ai blessé avec %i point" cards mana) |> Async.RunSynchronously
    
    (cards,p)

let arbiterSpeak = arbiter |> asyncSpeak

let name i = 
    displaySpeech arbiterSpeak (sprintf "Choisissez le nom du joueur %i : " i) |> Async.Start
    System.Console.ReadLine()

let mode i = 
    let rec internalMode () =
        displaySpeech arbiterSpeak (sprintf "Choisissez le mode du joueur %i\n\t1 - Computer\n\t2 - Player" i) |> Async.Start
        match System.Console.ReadLine() with
        | "1" -> kamikaze
        | "2" -> human
        | _ ->
            displaySpeech arbiterSpeak "Mode invalide" |> Async.RunSynchronously
            internalMode ()
    internalMode ()

let nameForTest i = 
    match i with
    | 1 -> "Foufou"
    | _ -> "Bourré"

let modeForTest _ = kamikaze

let game = startGame arbiterSpeak name mode

let endGame = play arbiterSpeak game 

match endGame with
| End g -> 
//    let message = sprintf "\n\nGame Over in %i turn! \nThe winner is %s\nYou loose %s !" g.pass g.looser.name g.winner.name
    let message = sprintf "\n\nFin de la partie en %i tours! \nLe gagnant est %s\nTu as perdu %s !" g.pass g.winner.name g.looser.name
    displaySpeech (arbiter |> asyncSpeak) message |> Async.RunSynchronously
    System.Console.ReadLine() |> ignore
| _ -> printfn "started"