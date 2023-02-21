-- Creator : Yagmur Saygili / School Assigment
-- Tree operations with recursive approaches on haskell

data Tree x =   Node x (Tree x) (Tree x) | Leaf x  deriving (Show,Eq,Ord)

positivetree x '+' = Leaf (head(head x))
--positivetree (a:b:c) '+' = if (a==0) 


--Height calculation------------------------------------------------------------------------
generateheight (Leaf root) = 1
--generateheight (Node root (Leaf a) (Leaf b)) = 2
generateheight (Node root left (Leaf a)) = 1 + generateheight left
generateheight (Node root (Leaf a) right) = 1+ generateheight right 
--Buna bir daha bakalım


---Level weight calculation------------------------------------------------------------------------
levelweight  (Leaf root) n = if (n==1) then root
                             else 0
levelweight  (Node root left (Leaf b) ) n = if (n==1) then root 
                                            else (levelweight left (n-1)) + (levelweight (Leaf b) (n-1))
                                            
levelweight  (Node root (Leaf a) right ) n = if (n==1) then root 
                                            else (levelweight (Leaf a) (n-1)) + (levelweight right (n-1))
                                            
                                            

                            
--Manual Traversal-------------------------------------------------

data Message = NodeNotFound | StoppedEarly | Found  deriving (Show,Read)

generateMessage x = if (x>0) then Found
                    else if (x<0) then NodeNotFound
                    else StoppedEarly
                    
--I tried to work with disjoint unions and typeclasses for desired output but I could not manage to do it

--Finding node part         
--This function returns a node if it can be reached or returns (Leaf 0) as sentinel value                   
findnode (Leaf a) number = if (a==number) then (Leaf number)
                           else (Leaf 0)--I send (Leaf 0) as sentinel value since I cannot find a node and check this condition in goleftorright function
                           
findnode (Node root left (Leaf b)) number = if (root==number) then (Node root left (Leaf b))
                                            else findnode (left) number
                                            
findnode (Node root (Leaf a) right) number = if (root==number) then (Node root (Leaf a) right)
                                            else findnode (right) number

 



--This function takes the specified node and string of direction as an input and shows the message
goleftorright  (Leaf 0) _ = putStrLn "NodeNotFound"
goleftorright (Node root left right) "" = putStrLn $ "Found " ++ show root 
goleftorright (Leaf a) "" =  putStrLn $ "StoppedEarly"
goleftorright (Leaf a) (x:xs)  = putStrLn "NodeNotFound" 
goleftorright (Node root left right) (x:xs)= if (x=='L') then goleftorright (left) xs
                                             else goleftorright (right) xs
--Because I could not find a way to both generate function properly and print messages accordingly I unfortunately kept the putStrLn function hocam, apologies


--This function takes the whole tree and string of directions and sends to the goleftorright function with specified node to traverse between nodes in that function
manualtraversal (Leaf a) number "" = putStrLn "NodeNotFound" -- This step is incase of user sends a leaf to this function directly
manualtraversal (Node root left right) number ch = goleftorright ( findnode (Node root left right) number) ch
                                                   