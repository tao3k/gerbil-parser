;;; -*- Gerbil -*-
;;; Compatibility facade; the canonical parser entry owns the public API.
(import ./parser)
(export +tla-plus-model-qualification-schema+
        qualify-tla-plus-model qualify-tla-plus-core-model
        tla-plus-model-receipt? tla-plus-model-receipt-admitted
        tla-plus-model-receipt-output tla-plus-model-receipt->alist)
