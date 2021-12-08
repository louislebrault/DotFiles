import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

-- se faire une barre avec xmobar
-- Déplacer les raccourcis workspace sur 7 8 9 0 - =
-- Trouver une places pour les raccourcis que j'avais rajouté sur i3 (colemak/azerty, chromium)
-- Lancer les commandes qu'i3 lancait á l'ouverture

main :: IO ()
main = xmonad $ def
    { modMask = mod4Mask  -- Rebind Mod to the Super key
    , terminal = "termite"
    }

