module ParseMode where 

import Language.Haskell.Exts


mode :: ParseMode
mode = (ParseMode {
        parseFilename = "<None>",
        baseLanguage = Haskell2010,
        extensions = filterEnable knownExtensions,
        ignoreLanguagePragmas = False,
        ignoreLinePragmas = True,
        fixities = Just baseFixities,
        ignoreFunctionArity = False
      })

filterEnable :: [Extension] -> [Extension]
filterEnable [] = []
filterEnable (ext@(EnableExtension  _):exts) = ext : filterEnable exts
filterEnable     ((DisableExtension _):exts) = filterEnable exts   