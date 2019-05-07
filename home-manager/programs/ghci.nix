{ lib, ... }:

{
  home.file.".ghci".text = lib.strings.concatStringsSep "\n" [
    '':set prompt "\ESC[0;34mghci>\ESC[m "''
    (let
      P = x: "Prelude." + x;
      return = P "return";
      asString = x: "(\"" + x + "\" :: " + P "String" + ")";
      cat = P "++";
      fun1 = arg: body: "\\ " + arg + " -> " + body;
      unwords = ws: lib.strings.concatStringsSep " " ws;
    # :def hoogle \s -> Prelude.return Prelude.$ (":! hoogle --count=15 \"" :: Prelude.String) Prelude.++ s Prelude.++ ("\"" :: Prelude.String)
    # :def hoogle \s -> return ((":! hoogle --count=15 \"" :: String) ++ s ++ ("\"" :: String))
    in
      ":def hoogle " +
      (fun1 "s" (unwords [
        return "(" (asString ":! hoogle --count=15 \"") cat "s" cat (asString "\"") ")"
      ]))
    )
    ":set -cpp -DASSERTS -DDEBUG"
    ":set -Wno-name-shadowing"
    ":set -XOverloadedStrings -XScopedTypeVariables -XTupleSections -XFlexibleContexts -XDataKinds"

    # -- :set +s
    # -- :set -XPartialTypeSignatures

    # -- import qualified Data.Text   as T
    # -- import qualified Data.Vector as V
    # -- import qualified Data.HashSet as HS
    # -- import qualified Data.HashMap.Strict as HM
    # -- import Data.Monoid ((<>))
  ];
}
