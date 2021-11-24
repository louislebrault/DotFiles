import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

-- Déplacer les raccourcis workspace sur 7 8 9 0 - =
-- Trouver une places pour les raccourcis que j'avais rajouté sur i3 (colemak/azerty, chromium)

main :: IO ()
main = xmonad $ def
    { modMask = mod4Mask  -- Rebind Mod to the Super key
    , terminal = "termite"
    }

