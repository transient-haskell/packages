browserProgram= onBrowser $ do
          name <- local $ render $ getString Nothing `fire` OnKeyUp
          r <- atRemote $ do
                         localIO $ print name
                         return $ "hello " ++ name

          local . render . rawHtml h1 r


at "#identifer" Insert $ rawHtml …     -- render widget anywhere in the web page