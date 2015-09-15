type Label = string

type VitalForce = {units:int}

let getVitalForce vitalForce = 
   let oneUnit = {units = 1}
   let remaining = {units = vitalForce.units-1}  // decrement
   oneUnit, remaining  // return both

type DeadLeftLeg = DeadLeftLeg of Label
type LiveLeftLeg = LiveLeftLeg of Label * VitalForce

type M<'LiveBodyPart> = 
    M of (VitalForce -> 'LiveBodyPart * VitalForce)

let makeLiveLeftLegM deadLeftLeg  = 
    let becomeAlive vitalForce = 
        let (DeadLeftLeg label) = deadLeftLeg
        let oneUnit, remainingVitalForce = getVitalForce vitalForce 
        let liveLeftLeg = LiveLeftLeg (label,oneUnit)
        liveLeftLeg, remainingVitalForce    
    // changed!        
    M becomeAlive // wrap the function in a single case union

let deadLeftLeg = DeadLeftLeg "Boris"
let leftLegM = makeLiveLeftLegM deadLeftLeg
let vf = {units = 10}
let (M innerFn) = leftLegM 

let runM (M f) vitalForce = f vitalForce
let liveLeftLeg, remainingAfterLeftLeg = runM leftLegM vf 

type DeadLeftBrokenArm = DeadLeftBrokenArm of Label 

// A live version of the broken arm.
type LiveLeftBrokenArm = LiveLeftBrokenArm of Label * VitalForce

// A live version of a heathly arm, with no dead version available
type LiveLeftArm = LiveLeftArm of Label * VitalForce

// An operation that can turn a broken left arm into a heathly left arm
type HealBrokenArm = LiveLeftBrokenArm -> LiveLeftArm

// implementation of HealBrokenArm
let healBrokenArm (LiveLeftBrokenArm (label,vf)) = LiveLeftArm (label,vf)

let mapM f bodyPartM = 
    let transformWhileAlive vitalForce = 
        let bodyPart,remainingVitalForce = runM bodyPartM vitalForce 
        let updatedBodyPart = f bodyPart
        updatedBodyPart, remainingVitalForce
    M transformWhileAlive

let healBrokenArmM = mapM healBrokenArm

let makeLiveLeftBrokenArm deadLeftBrokenArm = 
    let (DeadLeftBrokenArm label) = deadLeftBrokenArm
    let becomeAlive vitalForce = 
        let oneUnit, remainingVitalForce = getVitalForce vitalForce 
        let liveLeftBrokenArm = LiveLeftBrokenArm (label,oneUnit)
        liveLeftBrokenArm, remainingVitalForce    
    M becomeAlive

/// create a dead Left Broken Arm
let deadLeftBrokenArm = DeadLeftBrokenArm "Victor"

/// create a M<BrokenLeftArm> from the dead one
let leftBrokenArmM = makeLiveLeftBrokenArm deadLeftBrokenArm

let leftArmM = leftBrokenArmM |> mapM healBrokenArm  

type DeadRightLowerArm = DeadRightLowerArm of Label 
type DeadRightUpperArm = DeadRightUpperArm of Label 

type LiveRightLowerArm = LiveRightLowerArm of Label * VitalForce
type LiveRightUpperArm = LiveRightUpperArm of Label * VitalForce

// define the whole arm
type LiveRightArm = {
    lowerArm : LiveRightLowerArm
    upperArm : LiveRightUpperArm
    }

// surgery to combine the two arm parts
let armSurgery lowerArm upperArm =
    {lowerArm=lowerArm; upperArm=upperArm}

let map2M f m1 m2 =
    let becomeAlive vitalForce = 
        let v1,remainingVitalForce = runM m1 vitalForce 
        let v2,remainingVitalForce2 = runM m2 remainingVitalForce  
        let v3 = f v1 v2
        v3, remainingVitalForce2    
    M becomeAlive

let makeLiveRightLowerArm (DeadRightLowerArm label) = 
    let becomeAlive vitalForce = 
        let oneUnit, remainingVitalForce = getVitalForce vitalForce 
        let liveRightLowerArm = LiveRightLowerArm (label,oneUnit)
        liveRightLowerArm, remainingVitalForce    
    M becomeAlive

let makeLiveRightUpperArm (DeadRightUpperArm label) = 
    let becomeAlive vitalForce = 
        let oneUnit, remainingVitalForce = getVitalForce vitalForce 
        let liveRightUpperArm = LiveRightUpperArm (label,oneUnit)
        liveRightUpperArm, remainingVitalForce    
    M becomeAlive

let deadRightLowerArm = DeadRightLowerArm "Tom"
let lowerRightArmM = makeLiveRightLowerArm deadRightLowerArm 

let deadRightUpperArm = DeadRightUpperArm "Jerry"
let upperRightArmM = makeLiveRightUpperArm deadRightUpperArm

let armSurgeryM  = map2M armSurgery 
let rightArmM = armSurgeryM lowerRightArmM upperRightArmM 

type DeadBrain = DeadBrain of Label 
type Skull = Skull of Label 

type LiveBrain = LiveBrain of Label * VitalForce

type LiveHead = {
    brain : LiveBrain
    skull : Skull // not live
    }

let headSurgery brain skull =
    {brain=brain; skull=skull}

let returnM x = 
    let becomeAlive vitalForce = 
        x, vitalForce 
    M becomeAlive

let makeLiveBrain (DeadBrain label) = 
    let becomeAlive vitalForce = 
        let oneUnit, remainingVitalForce = getVitalForce vitalForce 
        let liveBrain = LiveBrain (label,oneUnit)
        liveBrain, remainingVitalForce    
    M becomeAlive

let deadBrain = DeadBrain "Abby Normal"
let skull = Skull "Yorick"

let liveBrainM = makeLiveBrain deadBrain
let skullM = returnM skull

let headSurgeryM = map2M headSurgery
let headM = headSurgeryM liveBrainM skullM

type DeadHeart = DeadHeart of Label 
type LiveHeart = LiveHeart of Label * VitalForce

type BeatingHeart = BeatingHeart of LiveHeart * VitalForce

let makeLiveHeart (DeadHeart label) = 
    let becomeAlive vitalForce = 
        let oneUnit, remainingVitalForce = getVitalForce vitalForce 
        let liveHeart = LiveHeart (label,oneUnit)
        liveHeart, remainingVitalForce    
    M becomeAlive

let makeBeatingHeart liveHeart = 

    let becomeAlive vitalForce = 
        let oneUnit, remainingVitalForce = getVitalForce vitalForce 
        let beatingHeart = BeatingHeart (liveHeart, oneUnit)
        beatingHeart, remainingVitalForce    
    M becomeAlive

let makeBeatingHeartFromLiveHeartM liveHeartM = 

    let becomeAlive vitalForce = 
        // extract the liveHeart from liveHeartM 
        let liveHeart, remainingVitalForce = runM liveHeartM vitalForce 

        // use the liveHeart to create a beatingHeartM
        let beatingHeartM = makeBeatingHeart liveHeart

        // run beatingHeartM to get a beatingHeart
        let beatingHeart, remainingVitalForce2 = runM beatingHeartM remainingVitalForce 

        // return a beatingHeart and remaining vital force    
        beatingHeart, remainingVitalForce2    

    // wrap the inner function and return it        
    M becomeAlive

let bindM f bodyPartM = 
    let becomeAlive vitalForce = 
        let bodyPart, remainingVitalForce = runM bodyPartM vitalForce 
        runM (f bodyPart) remainingVitalForce 
    M becomeAlive

let beatingHeartM = 
   DeadHeart "Anne"
   |> makeLiveHeart 
   |> bindM makeBeatingHeart


type LiveBody = {
    leftLeg: LiveLeftLeg
    rightLeg : LiveLeftLeg
    leftArm : LiveLeftArm
    rightArm : LiveRightArm
    head : LiveHead
    heart : BeatingHeart
    }

let applyM mf mx =
    let becomeAlive vitalForce = 
        let f,remainingVitalForce = runM mf vitalForce 
        let x,remainingVitalForce2 = runM mx remainingVitalForce  
        let y = f x
        y, remainingVitalForce2    
    M becomeAlive 

let createBody leftLeg rightLeg leftArm rightArm head beatingHeart =
    {
    leftLeg = leftLeg
    rightLeg = rightLeg
    leftArm = leftArm
    rightArm = rightArm
    head = head
    heart = beatingHeart 
    }

let rightLegM = leftLegM

let (<*>) = applyM

let (<!>) = mapM

let bodyM = 
    createBody 
    <!> leftLegM
    <*> rightLegM
    <*> leftArmM
    <*> rightArmM
    <*> headM 
    <*> beatingHeartM

let liveBody, remainingFromBody = runM bodyM vf