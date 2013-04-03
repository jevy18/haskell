import Data.Char

--@author: jevy18
--@language: haskell
--@version 1.1

addDigit::Int->Int->Int
addDigit a b
	    |a<10 = (a*10)+ b
            |otherwise = (a*10)+b

convert::Float->Float
convert a = (a*9/5)+32

mystery::Int->Int->Int->Bool
mystery a b c = not((a==b)&&(b==c))

threeDifferent::Int->Int->Int->Bool
threeDifferent a b c = (((a/=b)&&(a/=c))&&(b/=c))

maxThree::(Int,Int,Int)->Int
maxThree (a,b,c)
                |(a>=b)&&(a>=c) = a
                |(b>=a)&&(b>=c) = b
                |otherwise = c

time::Int->(Int,Int,Int)
time t = (a,b,c)
                where a= div t h
                      b= (mod t s)`div` s
                      c= mod t s
                      h= s*s
                      s= 60

sumSeries::Int->Int
sumSeries a
           |a==0 = 1
           |otherwise = sumSeries (a-1) + power a
           where
                power::Int->Int
                power a
                       |a==0 = 0
                       |otherwise = a^a

addInteger::Int->Int
addInteger a
            |a<10 = a
            |otherwise = addInteger (div a 10)+ (mod a 10)

count::Int->Int
count a
       |a<5 = a
       |otherwise = count(a-1)+count(a-2)+count(a-3)

countTrue::[Bool]->Int
countTrue [] = 0
countTrue (x:xs) = 1+length[x|x<-xs,x== True]

allSame::[Int]->Bool
allSame xs = and[head xs == x|x<-tail xs]

slit::Int->[Int]->[Int]
slit _ (x:[]) = (x:[]) --slit _ [] = [] -- Also the same thing
slit a [x] = [x]
slit a(x:xs) = take a (slit a [x] ++ xs)

-- implementation of the zip function in-built
zip_F::[Int]->[Int]->[(Int,Int)]
zip_F xs [] = []
zip_F [] ys = []
zip_F (x:xs)(y:ys) = (x,y): zip_F xs ys

-- implementation of the map function in-built
map_Fun f [] = []
map_Fun f(x:xs) = f x : map_Fun f xs

fin::[a]->Int
fin [] = 0
fin (x:xs) = 1+ fin xs

don::Double->Double->Double -- a new type signature
don a b = a+b

max2::Int->Int->Int
max2 a b
       |a>=b = a
       |otherwise = b
max3::Int->Int->Int->Int
max3 a b c
          |(a>b)&&(a>c) = a
          |(b>c)&&(b>c) = b
          |otherwise = c

add::Int->Int->Int
add a 0 = a
add a b = 1+add a(b-1)

plus::a->a
plus a = a

--Recursive function to add the head of both list.
sumVectors,sumVectors2::[Int]->[Int]->[Int]
sumVectors [] [] = []
sumVectors xs [] = xs
sumVectors [] ys = ys
sumVectors (x:xs)(y:ys)
                       |(x:xs==[])||(y:ys==[])=[]
                       |otherwise = x+y:sumVectors xs ys

sumVectors2 [] [] = []
sumVectors2 xs [] = xs
sumVectors2 [] ys = ys
sumVectors2 num1 num2 = [x+y|(x,y)<-zip num1 num2]


-- double all the elements within a list using high order function map
double::[Int]->[Int]
double xs = map (*2) xs

--The function test if all the element inside the list has the value zero;
-- producing the result to each element in the list with the usage of high order function map
allZero::[Int]->[Bool]
allZero [] = [False]
allZero (x:xs) = map (==0) (x:xs)

--The function test if all the elements inside the list is all zero.
-- High order function usage once again.
allZero2::[Int]->Bool
allZero2 xs = and(map(==0) xs)

bill::(Num a,Eq a)=>a->a->a --Num inbuilt class in haskell,which makes polymorhic definition possible for all types.The Eq class checks haskell inbuilt function defintions.
bill a b = a + b

digit::Int->(Int,Int)
digit a
       |a<10 = (a,a)
       |otherwise = (div a 10,mod a 10)

sub::[Int]->[Int]
sub [] = []
sub (x:xs)= 10-x : sub xs