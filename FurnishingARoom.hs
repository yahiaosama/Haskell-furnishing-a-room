import FurnitureResources

--findFurnitureUpdate: This function takes as an input an object,another object,the poistion of the 2nd object relative to the 1st one and the list of statistics of a room.It then sends the same inputs along with an empty accumulator to a helper function findFurnitureUpdate1.

findFurnitureUpdate a b c l = findFurnitureUpdate1 a b c l []

--findFurnitureUpdate1: A helper function for the findFurnitureUpdate function, it traverses the list of statistics until it finds an object corresponding to object 'a' and then checks the position of the 2nd object 'b' relative to 'a' to decide which position list in the list of statistics to send to the helper function updateHelper.It also appends the output coming from the updateHelper function with the accumulator when it finds object a matching an object in the list of statistics. 

findFurnitureUpdate1 a b c ((x,[y,z]):l) acc = if( a == x ) then if(c == "right") then acc++((x,[updateHelper a b c y ,z]):l)  else acc++((x,[y,updateHelper a b c z]):l) else findFurnitureUpdate1 a b c l (acc++[(x,[y,z])])

--If there is no match for object 'a' in the statistics list, this function just sends the object 'a' , 'b', and the position z with an empty position list corresponding to the position 'c' to the updateHelper function and appends the output to the accumulator along with the object 'a' itself and  the other position list being emtpy as well.

findFurnitureUpdate1 a b c [] acc = if (c == "right") then acc++[(a,[updateHelper a b c [],[]])] else acc++[(a,[[],updateHelper a b c []])]

--updateHelper: A helper function that updates the position list of the object 'a' by increasing the frequency of the object 'b' by 1 if it matches 'x'.

updateHelper a b c ((x,y,z):xs) = if( b == x ) then ((x,y,z+1):xs) else [(x,y,z)] ++ updateHelper a b c xs

--A base case for the helper function, if object 'b' does not exist in the position list of the object 'a' then it is just placed in the position list by a frequency of 1.

updateHelper a b c [] = [(b,c,1)]

--generate: It updates the list of statistics (statslist) l by calling the helper functions generateRight on the input list of lists and the statslist that came as the output of the helper function generateBelow called on the same input list of lists and the statslist l.

generate ((x:xs):ys) l = generateRight ((x:xs):ys) (generateBelow ((x:xs):ys) l) 

--generateRight: A helper function for the function generate, it deals with each list in the input list of lists separately to generate the right position list and adds it to the statslist by calling findFurnitureUpdate on every two adjacent objects in each list of the input list of lists.

generateRight ((x:y:xs):ys) l = generateRight ((y:xs):ys) (findFurnitureUpdate x y "right" l)

--This is the case when a list in the input list of lists has only one object remaining inside it so the function starts working with the next list in the input list of lists.

generateRight ((x:[]):ys) l = generateRight ys l 

--A base case for the helper function generateRight which indicates that if the input list of lists becomes empty after traversing all the lists inside it then the output is the updated statsList l. 

generateRight [] l = l


--generateBelow: Another helper function for the function generate, it sends the the first two lists of the input list of lists along with the statslist l to the function generateBelow1. It also clones the second list in the input list of lists along with its tail so that it can work on this second list in the next traversal.

generateBelow ((x:xs):(y:ys):z) l = generateBelow1 (x:xs) (y:ys)  ((y:ys):z) l

--A base case for the helper function generateBelow, if the last list in the input list of lists has been reached then the output is the updated statslist l.

generateBelow (x:[]) l = l

--generateBelow1: A helper function for the helper function generateBelow, it deals with each two adjacent lists in the input list of lists at the same time, traversing the corresponding indices in each two lists in the input list of lists to generate the below position list and updates the statslist l by calling findFurnitureUpdate on every two objects having the same index in both lists.

generateBelow1 (x:xs) (y:ys) z l = generateBelow1 xs ys z (findFurnitureUpdate x y "below" l)

--A base case for the helper function generateBelow1 when the two input lists become empty, it calls the helper function generateBelow sending to it the clone that has been already made and the statslist l.

generateBelow1 [] []  z l = generateBelow z l


--statsList: It is a function that outputs the list of statistics of each object throughout the rooms by using a high order function foldr and using generate as the function that foldr uses on the whole list.

statsList  = sortList (foldr (generate) [] training) 


--sortList: sorts the input list of statistics using insertion sort.

sortList x = sortList1 x []


-- sortList1: takes as an input the statistics list and an accumulator and then recursively calls itself on two inputs which are the tail of the statistics list and another helper function sortListHelper which takes two inputs itself; the head of the input statistics list and the accumulator.

sortList1 (x:xs) a = sortList1 xs (sortListHelper x a)

--A case when the input statistics becomes empty then the output is the accumulator itself.

sortList1 [] a = a

--sortListHelper: Takes as an input the first list in the statistics list of lists and the accumulator. It then appends the input list with its right position list sorted and the below poistion list sorted along with the accumulator. 

sortListHelper (x,[right,below]) a = a ++ [(x,[sortRight right [],sortBelow below []])]

--sortRight: Takes the right position list of the x sent by the sortListHelper before along with the accumulator. it checks if the accumulator is empty then it recursively calls itself on the tail of the right position list and the output resulting from appending the head of the right position list with the accumulator.If the accumulator is not empty then it recursively calls itself on the tail of the right position list and the output resulting from calling the other helper function sortHelper which is called on the head of the right position list and the accumulator to find its right position in the accumulator.

sortRight (x:xs) a = if(length a == 0) then sortRight xs (a++[x]) else sortRight xs (sortHelper x a)

--A base case for the helper function sortRight.When the right position list is empty then it returns the accumulator.

sortRight [] a = a

--sortBelow: Takes the below position list of the x sent by the sortListHelper before along with the accumulator. it checks if the accumulator is empty then it recursively calls itself on the tail of the below position list and the output resulting from appending the head of the below position list with the accumulator.If the accumulator is not empty then it recursively calls itself on the tail of the below position list and the output resulting from calling the other helper function sortHelper which is called on the head of the below position list and the accumulator to find its right position in the accumulator.

sortBelow (x:xs) a = if(length a == 0) then sortBelow xs (a++[x]) else sortBelow xs (sortHelper x a)

--A base case for the helper function sortBelow.When te below position list is empty then it returns the accumulator.

sortBelow [] a = a

--sortHelper: A helper function that takes the item and the accumulator sent from either sortRight function or sortBelow functoin and compares the frequencies to find the right position for the input item in the accumulator according to the frequency of the item.

sortHelper (a,b,c) ((x,y,z):xs) = if(c >= z ) then [(a,b,c)]++((x,y,z):xs) else [(x,y,z)]++sortHelper (a,b,c) xs

--A base case for the helper function sortHelper.When the accumulator is empty , it inserts the input item as the last item in the accumulator.

sortHelper (a,b,c) [] = [(a,b,c)]


--getFurnStat: A function that calls a helper function furnStatHelper sending to it two inputs: the object x and the list of statistics which is the output of the function statsList.

getFurnStat x = furnStatHelper x statsList


--furnStatHelper: A helper function for the function getFurnStat, it outputs the list of statistics of the object x if it finds a match between object 'x' and object 'y' while traversing the list of statistics.

furnStatHelper x ((y,l):ys) = if (x==y) then l else furnStatHelper x ys

--A base case for the helper funciton furnStatHelper, when the input object is not found in the statistics list then an error message appears telling the user that this piece if furniture is not found in the statistics list.

furnStatHelper _ [] = error "Unknown Furniture"

--getPossibleNeighbour: A function that aims to get the possible neighbour according to the two input lists.It calls a helper function chooseRandomly which is called on another helper function possibleNeighbourHelper which takes the two input lists and a empty accumulator in this case.

getPossibleNeighbour x y = chooseRandomly (possibleNeighbourHelper x y [])

--possibleNeighbourHelper: A helper function for the function getPossibleNeighbour that works on the item in each of the two lists where it uses the predefined function replicate to replicate the object x in the first list and object a in the second list by each object's frequency z and c respectively. It then appends these two replications with the accumulator and calls itself one more time to work on the tails of the lists and the updated accumulator.

possibleNeighbourHelper ((x,y,z):xs) ((a,b,c):ys) acc = possibleNeighbourHelper xs ys (acc++(replicate z x ++ replicate c a))

--A case for the helper function where the below list is empty so it only works on the right list.

possibleNeighbourHelper ((x,y,z):xs) [] acc = possibleNeighbourHelper xs [] (acc++replicate z x)

--A case for the helper function where the right list is empty so it only works on the below list.

possibleNeighbourHelper [] ((a,b,c):ys) acc = possibleNeighbourHelper [] ys (acc++replicate c a)

--A case for the helper function where both lists are empty then the output is the updated accumulator. 

possibleNeighbourHelper [] [] a = a

--chooseRandomly: A helper function to the getPossibleNeighbour function. It chooses a random element from the list x.

chooseRandomly x = x !! (randomZeroToX ((length x)-1))

--furnishRoom:It takes as an input the dimension of the room and a starting to object to furnish the room, it then passes the the dimension and the starting object along with an empty accumulator and an integer representing the row number.
 
furnishRoom x y = furnishHelper x y [] 0

--furnishHelper: A helper function to furnishRoom it takes as an input the dimension, starting object, accumulator and the current row.If the row is zero the recurses with row +1 and appending the accumulator with the row being furnished which is the output of furnishRight and since its the first row the it will be furnished right only, if the row is between 0 and x which is the dimension it recurses with row+1 and the accumulator which is the result appended with the output of furnishRightBelow which outputs the row being furnished with respect to the same row and the row before it ( top of it) otherwise if the row is X it outputs the resulting list.

furnishHelper x y a row 
			|row == 0 = furnishHelper x y (a++[furnishRight x [y] 1]) (row+1)
			|row > 0 && row <x = furnishHelper x y (a++[furnishRightBelow x [] (last a) 0]) (row+1)
			|x == 0 = []
			|otherwise = a

--furnishRight:A helper function that furnish the room with respect to the left object only, this function takes as an input x which is the dimension and a list which is the list being furnished and index, if the index is equal to the dimension then this list is done furnished it the returns the accumulator (base case) ,otherwise it recurses with (index+1) and the accumulator appended with the element that is a possible neighbour to the left element in the accumulator (row being furnished)

furnishRight x a index 
			|index == x = a
			|otherwise =  furnishRight x (a++[getPossibleNeighbour k []]) (index+1)       
			where k = (convertHelper (head (getFurnStat (last a))))

--furnishRightBelow:A helper function the furnish the room with respect to the left element and the upper element, this function takes as an input the dimension and the (current) list being furnished the list before this list (upper list) , if the index is zero then the function recurses with (index+1) with the accumulator appended with the object that is a possible neighbour to the object on top of it only, else if the index is between 0 and the dimension then it recurses with (index+1) and the accumulator appended with object that is a possible neighbour to the object on top of it and the left object otherwise that(index is x or bigger) then it returns the accumulator (base case).

furnishRightBelow x current below index 
					 |index == 0 = furnishRightBelow x (current ++[getPossibleNeighbour [] b]) below (index+1)     
					 |index < x && index > 0 = furnishRightBelow x (current ++[getPossibleNeighbour k l]) below (index+1)	    
					 | otherwise = current
					 
					 	where (b,k,l) = ((convertHelper (last (getFurnStat (below !! index)))),(convertHelper (head (getFurnStat (current!!(index-1))))),(convertHelper (last (getFurnStat (below !! index)))))
 
	
						 						



--convertHelper: converts the third element of the pair(a,b,c) from Integer to Int to match getPossibleNeighbour type.

convertHelper l = map(\(a,b,c)->(a,b,fromIntegral c)) l 





