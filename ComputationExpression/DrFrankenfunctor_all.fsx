(*
monadster.fsx

Demonstrates how the state monad works
See also monadster2.fsx for the refactored version using computation expressions.

Related blog post: http://fsharpforfunandprofit.com/posts/monadster/


*)

// =================================================================================
// The Common Context (Label,VitalForce)
// =================================================================================

/// All body parts have a label
type Label = string

/// The Animal Electricity needed to create a live part
type VitalForce = {units:int}

// get one unit of vital force and return the unit and the remaining
let getVitalForce vitalForce = 
    let oneUnit = {units = 1}
    let remaining = {units = vitalForce.units-1}  // decrement
    oneUnit, remaining  // return both

// =================================================================================
// The Left Leg 
// =================================================================================

// Dr Frankenfunctor has a dead left leg lying around in the lab
type DeadLeftLeg = DeadLeftLeg of Label 

// and can make a live left leg from it
type LiveLeftLeg = LiveLeftLeg of Label * VitalForce

// how to make a live thing?  -- First approach
module Approach1 = 

    // version 1 -- the input is a tuple of DeadLeftLeg * VitalForce 
    type MakeLiveLeftLeg = 
        DeadLeftLeg * VitalForce -> LiveLeftLeg * VitalForce 

    let makeLiveLeftLeg (deadLeftLeg,vitalForce) = 
        // get the label from the dead leg using pattern matching
        let (DeadLeftLeg label) = deadLeftLeg
        // get one unit of vital force
        let oneUnit, remainingVitalForce = getVitalForce vitalForce 
        // create a live leg from the label and vital force
        let liveLeftLeg = LiveLeftLeg (label,oneUnit)
        // return the leg and the remaining vital force
        liveLeftLeg, remainingVitalForce    


// version 2 -- the input is a curried version. 
module Approach2 = 

    // First param is DeadLeftLeg, and then VitalForce is a separate param
    type MakeLiveLeftLeg = 
        DeadLeftLeg -> VitalForce -> LiveLeftLeg * VitalForce 

    let makeLiveLeftLeg deadLeftLeg vitalForce = 
        let (DeadLeftLeg label) = deadLeftLeg
        let oneUnit, remainingVitalForce = getVitalForce vitalForce 
        let liveLeftLeg = LiveLeftLeg (label,oneUnit)
        liveLeftLeg, remainingVitalForce    

// version 3 -- the input is a DeadLeftLeg, returning a generator function
module Approach3 = 

    type MakeLiveLeftLeg = 
        DeadLeftLeg -> (VitalForce -> LiveLeftLeg * VitalForce)

    let makeLiveLeftLeg deadLeftLeg = 
        // create an inner intermediate function
        let becomeAlive vitalForce = 
            let (DeadLeftLeg label) = deadLeftLeg
            let oneUnit, remainingVitalForce = getVitalForce vitalForce 
            let liveLeftLeg = LiveLeftLeg (label,oneUnit)
            liveLeftLeg, remainingVitalForce    
        // return it
        becomeAlive 


// Demonstrates how currying works
module CurryingExample = 

    // currying example - two parameters
    let add_v1 x y = 
        x + y

    // currying example - one parameter
    let add_v2 x = 
        fun y -> x + y

    // currying example - intermediate function
    let add_v3 x = 
        let addX y = x + y
        addX // return the function


// =================================================================================
// Creating the Monadster type
// =================================================================================


// version 4 -- make the function generic
module Approach4 = 

    type M<'LiveBodyPart> = 
        VitalForce -> 'LiveBodyPart * VitalForce

    let makeLiveLeftLeg deadLeftLeg :M<LiveLeftLeg> = 
        let becomeAlive vitalForce = 
            let (DeadLeftLeg label) = deadLeftLeg
            let oneUnit, remainingVitalForce = getVitalForce vitalForce 
            let liveLeftLeg = LiveLeftLeg (label,oneUnit)
            liveLeftLeg, remainingVitalForce    
        becomeAlive


// final version -- wrap Monadster body part recipe with "M"
type M<'LiveBodyPart> = 
    M of (VitalForce -> 'LiveBodyPart * VitalForce)

// the creation function looks like this now
let makeLiveLeftLegM deadLeftLeg  = 
    let becomeAlive vitalForce = 
        let (DeadLeftLeg label) = deadLeftLeg
        let oneUnit, remainingVitalForce = getVitalForce vitalForce 
        let liveLeftLeg = LiveLeftLeg (label,oneUnit)
        liveLeftLeg, remainingVitalForce    
    M becomeAlive  // wrap the function in a single case union

// and the function signature is:
// val makeLiveLeftLegM : DeadLeftLeg -> M<LiveLeftLeg>

// ---------------------------------------------------------------------------------
// Testing the left leg
// ---------------------------------------------------------------------------------


/// create Left Leg
let deadLeftLeg = DeadLeftLeg "Boris"
let leftLegM = makeLiveLeftLegM deadLeftLeg

// pretend that vital force is available
let vf = {units = 10}

let (M innerFn) = leftLegM 
let liveLeftLeg, remainingAfterLeftLeg = innerFn vf
//val liveLeftLeg : LiveLeftLeg = 
//    LiveLeftLeg ("Boris",{units = 1;})
//val remainingAfterLeftLeg : VitalForce = 
//    {units = 9;}

// encapsulate the function call that "runs" the recipe
let runM (M f) vitalForce = f vitalForce 

let liveLeftLeg2, remainingAfterLeftLeg2 = runM leftLegM vf  

// =================================================================================
// The Right Leg
// =================================================================================

// no right legs were available -- see the definition of LiveBody later for the workaround

// =================================================================================
// The Left Arm
// =================================================================================

// Dr Frankenfunctor has a dead but broken left arm lying around in the lab
type DeadLeftBrokenArm = DeadLeftBrokenArm of Label 

// You can have a live version of the broken arm too.
type LiveLeftBrokenArm = LiveLeftBrokenArm of Label * VitalForce

// There is a live version of a heathly arm, but no dead version
type LiveLeftArm = LiveLeftArm of Label * VitalForce

// However, Dr Frankenfunctor CAN turn a broken left arm into a heathly left arm
type HealBrokenArm = LiveLeftBrokenArm -> LiveLeftArm 

// implementation of HealBrokenArm 
let healBrokenArm (LiveLeftBrokenArm (label,vf)) = LiveLeftArm (label,vf)

// version 1 - explicit, hard-coded arm type
module HealArm_v1 = 

    /// convert a M<LiveLeftBrokenArm> into a M<LiveLeftArm>
    let makeHealedLeftArm brokenArmM = 

        // create a new inner function that takes a vitalForce parameter
        let healWhileAlive vitalForce = 
            // run the incoming brokenArmM with the vitalForce 
            // to get a broken arm
            let brokenArm,remainingVitalForce = runM brokenArmM vitalForce 
            
            // heal the broken arm
            let healedArm = healBrokenArm brokenArm

            // return the healed arm and the remaining VitalForce
            healedArm, remainingVitalForce

        // wrap the inner function and return it
        M healWhileAlive  

    /// Make generic by passing in a "f" to do the transform
    /// As a result, it convert a M<'a> into a M<'b>
    let makeGenericTransform f brokenArmM = 

        // create a new inner function that takes a vitalForce parameter
        let healWhileAlive vitalForce = 
            let brokenArm,remainingVitalForce = runM brokenArmM vitalForce 
            
            // heal the broken arm using passed in f
            let healedArm = f brokenArm
            healedArm, remainingVitalForce

        M healWhileAlive  

// ---------------------------------------------------------------------------------
// Introducing mapM
// ---------------------------------------------------------------------------------

// A generic map that works with ANY body part
let mapM f bodyPartM = 
    let transformWhileAlive vitalForce = 
        let bodyPart,remainingVitalForce = runM bodyPartM vitalForce 
        let updatedBodyPart = f bodyPart
        updatedBodyPart, remainingVitalForce
    M transformWhileAlive 

// signature
// mapM : f:('a -> 'b) -> M<'a> -> M<'b>

// so final version is simple
let healBrokenArmM = mapM healBrokenArm

// ---------------------------------------------------------------------------------
// The importance of mapM
// ---------------------------------------------------------------------------------

// some examples of map
module TheImportanceOfMap = 

    // map works with options
    let healBrokenArmO = Option.map healBrokenArm
    // LiveLeftBrokenArm option -> LiveLeftArm option

    // map works with lists
    let healBrokenArmL = List.map healBrokenArm
    // LiveLeftBrokenArm list -> LiveLeftArm list

// conversely, mapM will work with ANY normal type
module MapMWorksWithAllTypes = 

    let isEven x = (x%2 = 0)   // int -> bool
    // map it
    let isEvenM = mapM isEven  // M<int> -> M<bool>

    let isEmpty x = (String.length x)=0  // string -> bool
    // map it
    let isEmptyM = mapM isEmpty          // M<string> -> M<bool>

// ---------------------------------------------------------------------------------
// Testing the left arm
// ---------------------------------------------------------------------------------

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

/// create a M<LeftArm> using mapM and healBrokenArm 
let leftArmM = leftBrokenArmM  |> mapM healBrokenArm 


// now we can run it with the vital force
//let vf = {units = 10}
let liveLeftArm, remainingAfterLeftArm = runM leftArmM vf
//val liveLeftArm : LiveLeftArm = LiveLeftArm ("Victor",{units = 1;})
//val remainingAfterLeftArm : VitalForce = {units = 9;}


// =================================================================================
// The Right Arm
// =================================================================================

// Dr Frankenfunctor has TWO bits of a right arm, not a whole one
type DeadRightLowerArm = DeadRightLowerArm of Label 
type DeadRightUpperArm = DeadRightUpperArm of Label 

// which she can turn into LIVE ones
type LiveRightLowerArm = LiveRightLowerArm of Label * VitalForce
type LiveRightUpperArm = LiveRightUpperArm of Label * VitalForce

// and then combine the two live parts to make a whole arm
type LiveRightArm = {
    lowerArm : LiveRightLowerArm
    upperArm : LiveRightUpperArm
    }

let armSurgery lowerArm upperArm =
    {lowerArm=lowerArm; upperArm=upperArm}

/// convert a M<LiveRightLowerArm> and  M<LiveRightUpperArm> into a M<LiveRightArm>
let makeArmSurgeryM_v1 lowerArmM upperArmM =

    // create a new inner function that takes a vitalForce parameter
    let becomeAlive vitalForce = 
        // run the incoming lowerArmM with the vitalForce 
        // to get the lower arm
        let liveLowerArm,remainingVitalForce = runM lowerArmM vitalForce 
        
        // run the incoming upperArmM with the remainingVitalForce 
        // to get the upper arm
        let liveUpperArm,remainingVitalForce2 = runM upperArmM remainingVitalForce 

        // do the surgery to create a liveRightArm
        let liveRightArm = armSurgery liveLowerArm liveUpperArm

        // return the whole arm and the SECOND remaining VitalForce
        liveRightArm, remainingVitalForce2  
          
    // wrap the inner function and return it
    M becomeAlive  

// This has the correct signature
// M<LiveRightLowerArm> -> M<LiveRightUpperArm> -> M<LiveRightArm>

// ---------------------------------------------------------------------------------
// Introducing map2M
// ---------------------------------------------------------------------------------

// Here is a generic version
let map2M f m1 m2 =
    let becomeAlive vitalForce = 
        let v1,remainingVitalForce = runM m1 vitalForce 
        let v2,remainingVitalForce2 = runM m2 remainingVitalForce  
        let v3 = f v1 v2
        v3, remainingVitalForce2    
    M becomeAlive  
// f:('a -> 'b -> 'c) -> M<'a> -> M<'b> -> M<'c>

// we can then define armSurgeryM using map2M
// let armSurgeryM  = map2M armSurgery 


// ---------------------------------------------------------------------------------
// Testing the right arm
// ---------------------------------------------------------------------------------

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


// create the parts
let deadRightLowerArm = DeadRightLowerArm "Tom"
let lowerRightArmM = makeLiveRightLowerArm deadRightLowerArm 

let deadRightUpperArm = DeadRightUpperArm "Jerry"
let upperRightArmM = makeLiveRightUpperArm deadRightUpperArm

// create a function to make a whole arm
let armSurgeryM  = map2M armSurgery 
let rightArmM = armSurgeryM lowerRightArmM upperRightArmM 

let liveRightArm, remainingFromRightArm = runM rightArmM vf  
//val liveRightArm : LiveRightArm =
//  {lowerArm = LiveRightLowerArm ("Tom",{units = 1;});
//   upperArm = LiveRightUpperArm ("Jerry",{units = 1;});}
//val remainingFromRightArm : VitalForce = {units = 8;}


// =================================================================================
// The Head
// =================================================================================

// Dr Frankenfunctor has a dead brain and a skull
type DeadBrain = DeadBrain of Label 
type Skull = Skull  of Label 

// Only the brain needs to be made live
type LiveBrain = LiveBrain of Label * VitalForce

// and then the live brain is combined with the skull to make a head
type LiveHead = {
    brain : LiveBrain
    skull : Skull // not live
    }

let headSurgery brain skull =
    {brain=brain; skull=skull}


// Now we need an M<Skull> 
// But the Skull doesn't need any vital force.
// We just need to "lift" it into the world of M<_>

let wrapSkullInM skull = 
    let becomeAlive vitalForce = 
        skull, vitalForce 
    M becomeAlive
// val wrapSkullInM : 'a -> M<'a>

// ---------------------------------------------------------------------------------
// Introducing returnM
// ---------------------------------------------------------------------------------

// generic version
let returnM x = 
    let becomeAlive vitalForce = 
        x, vitalForce 
    M becomeAlive
// val returnM : 'a -> M<'a>


// ---------------------------------------------------------------------------------
// Testing the head
// ---------------------------------------------------------------------------------

let makeLiveBrain (DeadBrain label) = 
    let becomeAlive vitalForce = 
        let oneUnit, remainingVitalForce = getVitalForce vitalForce 
        let liveBrain = LiveBrain (label,oneUnit)
        liveBrain, remainingVitalForce    
    M becomeAlive


// create the dead parts
let deadBrain = DeadBrain "Abby Normal"
let skull = Skull "Yorick"

// create "M" versions
let liveBrainM = makeLiveBrain deadBrain
let skullM = returnM skull

// combine the parts
let headSurgeryM = map2M headSurgery
let headM = headSurgeryM liveBrainM skullM

// run the head with vital force
let liveHead, remainingFromHead = runM headM vf
//val liveHead : LiveHead = {brain = LiveBrain ("Abby normal",{units = 1;});
//                           skull = Skull "Yorick";}
//val remainingFromHead : VitalForce = {units = 9;}



// =================================================================================
// The Beating Heart 
// =================================================================================

// Dr Frankenfunctor has a dead heart
type DeadHeart = DeadHeart of Label 

// First, a live heart needs to be made
type LiveHeart = LiveHeart of Label * VitalForce

// and then a beating heart must be made from a LiveHeart 
// and some more vital force
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

//val makeLiveHeart : DeadHeart -> M<LiveHeart>
//val makeBeatingHeart : LiveHeart -> M<BeatingHeart>


// ---------------------------------------------------------------------------------
// how to connect the two M-generating functions?
// ---------------------------------------------------------------------------------

// problem is, I can only get a live heart from inside an M,
// not on its own :(

(*
let makeBeatingHeartFromLiveHeartM liveHeartM = 

    let becomeAlive vitalForce = 
        // extract the liveHeart from liveHeartM 
        let liveHeart, remainingVitalForce = runM liveHeartM vitalForce 

        // use the liveHeart to create a beatingHeartM
        let beatingHeartM = makeBeatingHeart liveHeart

        // what goes here?

        // return a beatingHeart and remaining vital force    
        beatingHeart, remainingVitalForce    

    // wrap the inner function and return it    
    M becomeAlive 
*)

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

// ---------------------------------------------------------------------------------
// Introducing bindM
// ---------------------------------------------------------------------------------

let bindM f bodyPartM = 

    let becomeAlive vitalForce = 
        let bodyPart, remainingVitalForce = runM bodyPartM vitalForce 
        let newBodyPartM = f bodyPart 
        let newBodyPart, remainingVitalForce2 = runM newBodyPartM remainingVitalForce 
        newBodyPart, remainingVitalForce2    
    M becomeAlive

// val bindM : f:('a -> M<'b>) -> M<'a> -> M<'b>

// alternate version
let bindM' f bodyPartM = 

    let becomeAlive vitalForce = 
        let bodyPart, remainingVitalForce = runM bodyPartM vitalForce 
        runM (f bodyPart) remainingVitalForce 
    M becomeAlive

// bindM in use
(*
// create a dead heart
let deadHeart = DeadHeart "Anne"

// create a live heart generator (M<LiveHeart>)
let liveHeartM = makeLiveHeart deadHeart

// create a beating heart generator (M<BeatingHeart>)
// from liveHeartM and the makeBeatingHeart function
let beatingHeartM = bindM makeBeatingHeart liveHeartM 
*)

// or simpler
(*
let beatingHeartM =
    DeadHeart "Anne" 
    |> makeLiveHeart 
    |> bindM makeBeatingHeart 
*)



// ---------------------------------------------------------------------------------
// Testing the heart
// ---------------------------------------------------------------------------------


let beatingHeartM =
    DeadHeart "Anne" 
    |> makeLiveHeart 
    |> bindM makeBeatingHeart 

let beatingHeart, remainingFromHeart = runM beatingHeartM vf

//val beatingHeart : BeatingHeart = 
//    BeatingHeart ("Anne",{units = 2;})
//val remainingFromHeart : VitalForce = 
//    {units = 8;}



// =================================================================================
// The whole body
// =================================================================================


// the live body is assembled from the subcomponents
type LiveBody = {
    leftLeg: LiveLeftLeg
    rightLeg : LiveLeftLeg
    leftArm : LiveLeftArm
    rightArm : LiveRightArm
    head : LiveHead
    heart : BeatingHeart
    }

// how to create this type? there are 6 fields!

// we could create a series of map functions.
// e.g. map3M looks like this
let map3M f m1 m2 m3 =
    let becomeAlive vitalForce = 
        let v1,remainingVitalForce = runM m1 vitalForce 
        let v2,remainingVitalForce2 = runM m2 remainingVitalForce  
        let v3,remainingVitalForce3 = runM m3 remainingVitalForce2  
        let v4 = f v1 v2 v3
        v4, remainingVitalForce3    
    M becomeAlive  

// but that is tedious.

// ---------------------------------------------------------------------------------
// Introducing applyM
// ---------------------------------------------------------------------------------

// let's use a generic way

let applyM mf mx =
    let becomeAlive vitalForce = 
        let f,remainingVitalForce = runM mf vitalForce 
        let x,remainingVitalForce2 = runM mx remainingVitalForce  
        let y = f x
        y, remainingVitalForce2    
    M becomeAlive  

// val applyM : M<('a -> 'b)> -> M<'a> -> M<'b>

// a function to create the body
let createBody leftLeg rightLeg leftArm rightArm head beatingHeart =
    {
    leftLeg = leftLeg
    rightLeg = rightLeg
    leftArm = leftArm
    rightArm = rightArm
    head = head
    heart = beatingHeart 
    }
// val createBody : LiveLeftLeg -> LiveLeftLeg -> LiveLeftArm -> LiveRightArm -> LiveHead -> BeatingHeart -> LiveBody

// clone the left leg
let rightLegM = leftLegM


// this is an example of using applyM, but in a ugly way
module UglyApplicativeExample = 
    let sixParamM = returnM createBody           // move to M-world
    let fiveParamM = applyM sixParamM leftLegM   // apply first M-param
    let fourParamM = applyM fiveParamM rightLegM // apply second M-param
    let threeParamM = applyM fourParamM leftArmM
    let twoParamM = applyM threeParamM rightArmM
    let oneParamM = applyM twoParamM headM 
    let bodyM = applyM oneParamM beatingHeartM   // result is a M<LiveBody>

// short cut
let (<*>) = applyM

// this is an example of using applyM in a nicer way
module InfixApplicativeExample = 
    let bodyM = 
        returnM createBody 
        <*> leftLegM
        <*> rightLegM
        <*> leftArmM
        <*> rightArmM
        <*> headM 
        <*> beatingHeartM

// another short cut 
let (<!>) = mapM


let bodyM = 
    createBody 
    <!> leftLegM
    <*> rightLegM
    <*> leftArmM
    <*> rightArmM
    <*> headM 
    <*> beatingHeartM

// ---------------------------------------------------------------------------------
// Testing the whole body
// ---------------------------------------------------------------------------------

// It's alive!
let liveBody, remainingFromBody = runM bodyM vf  

//val liveBody : LiveBody =
//  {leftLeg = LiveLeftLeg ("Boris",{units = 1;});
//   rightLeg = LiveLeftLeg ("Boris",{units = 1;});
//   leftArm = LiveLeftArm ("Victor",{units = 1;});
//   rightArm = {lowerArm = LiveRightLowerArm ("Tom",{units = 1;});
//               upperArm = LiveRightUpperArm ("Jerry",{units = 1;});};
//   head = {brain = LiveBrain ("Abby Normal",{units = 1;});
//           skull = Skull "Yorick";};
//   heart = BeatingHeart (LiveHeart ("Anne",{units = 1;}),{units = 1;});}

//val remainingFromBody : VitalForce = {units = 2;}