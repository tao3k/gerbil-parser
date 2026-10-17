import ParserEngine.PhysicalCoreExtraction

/-! Name discovery consumes the actual dense symbol projection, not an assumed
per-core pairing. EOF/completed #f slots do not allocate name IDs. -/
namespace ParserEngine.PhysicalSymbolDiscovery
open Grammar PhysicalCoreExtraction

theorem name_stream (g : CFG) :
    (physicalSymbols g).filterMap (fun symbol => symbol.bind NameDiscovery.nameOf) = NameDiscovery.stream g := by
  induction g with
  | nil => rfl
  | cons rule rest ih =>
    change (((rule.rhs.map some ++ [none]) ++ physicalSymbols rest).filterMap _) =
      rule.rhs.filterMap NameDiscovery.nameOf ++ NameDiscovery.stream rest
    simp [ih, Function.comp_def]

theorem discovered_from_physical (g : CFG) :
    NameDiscovery.discover NameDiscovery.empty
      ((physicalSymbols g).filterMap (fun symbol => symbol.bind NameDiscovery.nameOf)) = NameDiscovery.catalogue g := by
  rw [name_stream]; rfl

end ParserEngine.PhysicalSymbolDiscovery
