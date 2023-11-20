module Data where

  data Student = Student { 
                           name         :: String 
                         , grades       :: [Int]
                         , courses      :: [String]
                         , certificates :: [String]
                         } deriving Show

  data Batches = Batch Student Batches | Closes deriving Show


  learner1 :: Student
  learner1 = Student { name         = "Roberto"
                     , grades       = [83,74,99,92]
                     , courses      = ["FOB", "CDP", "HDP", "CSA"]
                     , certificates = []
                     }
  learner2 = Student { name         = "Brian"
                     , grades       = [95,98,92,100]
                     , courses      = ["HDP","CSA"]
                     , certificates = []
                     }
  learner3 = Student { name         = "Carlos"
                     , grades       = [83,74,79,80]
                     , courses      = ["FOB", "CDP"]
                     , certificates = []
                     }
  
  learner4 = Student { name         = "Alice"
                     , grades       = [95,98,92,100]
                     , courses      = ["HDP","CSA"]
                     , certificates = []
                     }
  learner5 = Student { name         = "Clarice"
                     , grades       = [83,74,79,80]
                     , courses      = ["FOB", "CDP"]
                     , certificates = []
                     }
  learner6 = Student { name         = "Miguel"
                     , grades       = [95,98,92,100]
                     , courses      = ["HDP","CSA"]
                     , certificates = []
                     }
  learner7 = Student { name         = "Hanibal"
                     , grades       = [83,74,79,80]
                     , courses      = ["FOB", "CDP"]
                     , certificates = []
                     }

  
  batch100 :: Batches
  batch100 = Batch learner1 $ Batch learner2 $ Batch learner3 Closes

  batch101 :: Batches
  batch101 = Batch learner4 $ Batch learner5 Closes

  batch102 :: Batches
  batch102 = Batch learner6 $ Batch learner7 Closes


