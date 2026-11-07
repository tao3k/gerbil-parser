// Private raw terminal publication and observer retention; not a Scheme heap model.
module OwnedTerminal {
  type Slot = {raw:bool,published:bool,label:int,visible:int,observed:bool}
  var slot:Slot
  pure val initial:Slot = {raw:true,published:false,label:0,visible:0,observed:false}
  action init = slot' = initial
  action Publish = all {not(slot.published),
    slot' = {...slot,raw:false,published:true,visible:slot.label}}
  action Observe = all {slot.published,
    slot' = {...slot,label:7,visible:7,observed:true}}
  action ShiftAgain = slot' = initial
  action step = any {Publish,Observe,ShiftAgain}
  val PublicationSafety = (slot.published implies not(slot.raw))
    and (slot.observed implies slot.label == slot.visible)
  // A discarded wrapper would erase an observer's label at the next reduction.
  action WrongRebox = all {slot.published,slot.observed,
    slot' = {...slot,label:0}}
  run privateTerminalTest = init.expect(slot.raw and not(slot.published) and PublicationSafety)
  run promotedObserverRetainedTest = init.then(Publish).then(Observe)
    .expect(PublicationSafety and not(slot.raw) and slot.label == 7)
  run reconstructedWrapperRejectedTest = init.then(Publish).then(Observe).then(WrongRebox)
    .expect(not(PublicationSafety))
}
