type Label = string

type VitalForce = {units:int}

let getVitalForce vitalForce = 
   let oneUnit = {units = 1}
   let remaining = {units = vitalForce.units-1}  // decrement
   oneUnit, remaining  // return both

type DeadLeftLeg = DeadLeftLeg of Label
type LiveLeftLeg = LiveLeftLeg of Label * VitalForce

type MakeLiveLeftLeg = 
    DeadLeftLeg -> (VitalForce -> LiveLeftLeg * VitalForce)

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
