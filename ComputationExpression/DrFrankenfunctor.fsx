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