! Advent of Code 2015 Day 15: Science for Hungry People
! https://adventofcode.com/2015/day/15

Module Types
!> Data type to store properties of ingredients
!
Type Ingredient
  Character (Len=32) :: ingrname
  Integer :: capacity
  Integer :: durability
  Integer :: flavour
  Integer :: textur
  Integer :: calories
End Type Ingredient
End Module Types


Program ScienceForHungryPeople
Use Types
Implicit None
Character(Len=256), Dimension(2) :: ExampleIngredients
Type (Ingredient), Dimension(48) :: ingredients
Character(Len=256), Dimension(64) :: inputlines
Integer :: i, numingr, bestcookie, calories
Integer, Dimension (48) :: mix
Integer :: CookieScore, FindBestRecipe

Write (*,'(A)') "--- Beispiel ohne Kalorienrechnung ---"
ExampleIngredients (1) = &
  & "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8"
ExampleIngredients (2) = &
  & "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"
Call ReadIngredients (2, ExampleIngredients, ingredients)
numingr = 2
Do i = 1, numingr
  Write (*,'(A,I2,A,A24,5(", ",A,": ",I4))') "Ingredient ", i, &
    & ": ", Trim (ingredients(i)%ingrname), &
    & "capacity", ingredients(i)%capacity, &
    & "durability", ingredients(i)%durability, &
    & "flavour", ingredients(i)%flavour, &
    & "texture", ingredients(i)%textur, &
    & "calories", ingredients(i)%calories
End Do
bestcookie = FindBestRecipe (numingr, ingredients, 100, mix, 0)
Call PrintRecipe (numingr, mix, ingredients)
Write (*,'(A,I12)') "Best cookie has score: ", bestcookie
Write (*,'(A)') ""

Write (*,'(A)') "--- Beispiel mit Randbedingung 500 Kalorien ---"
bestcookie = FindBestRecipe (numingr, ingredients, 100, mix, 500)
Call PrintRecipe (numingr, mix, ingredients)
Write (*,'(A,I12)') "Best cookie has score: ", bestcookie
Write (*,'()')

Write (*,'(A)') "--- Aufgabe 1: Bestes Rezept, ohne Rücksicht auf Kalorien ---"
Call ReadInputFile ("15-science-for-hungry-people-input.txt", numingr, inputlines)
Call ReadIngredients (numingr, inputlines, ingredients)
Write (*,'(A,I2)') "Read ingredients: ", numingr
Do i = 1, numingr
  Write (*,'(A,I2,A,A16,5(", ",A,": ",I4))') "Ingredient ", i, &
    & ": ", ingredients(i)%ingrname, &
    & "capacity", ingredients(i)%capacity, &
    & "durability", ingredients(i)%durability, &
    & "flavour", ingredients(i)%flavour, &
    & "texture", ingredients(i)%textur, &
    & "calories", ingredients(i)%calories
End Do
bestcookie = FindBestRecipe (numingr, ingredients, 100, mix, 0)
Call PrintRecipe (numingr, mix, ingredients)
Write (*,'(A,I12)') "Best cookie has score: ", bestcookie
Write (*,'()')

Write (*,'(A)') "--- Aufgabe 2: Bestes Rezept mit 500 Kalorien ---"
bestcookie = FindBestRecipe (numingr, ingredients, 100, mix, 500)
Call PrintRecipe (numingr, mix, ingredients)
Write (*,'(A,I12)') "Best cookie has score: ", bestcookie
End Program ScienceForHungryPeople

!> Read file contents
!
Subroutine ReadInputFile (filename, numlines, lines)
  Character(Len=*), Intent(In) :: filename
  Integer, Intent(Out) :: numlines
  Character(Len=256), Dimension(64), Intent(Out) :: lines
  Character(Len=256) :: s
  Integer :: readok
  Open (unit=11, file=filename, status='OLD', action='READ', iostat=readok)
  numlines = 0
  readok = 0
  Do While (readok .eq. 0)
    Read (11, '(A)', iostat=readok) s
    If (readok .ne. 0)  Exit
    numlines = numlines + 1
    lines (numlines) = s
  End Do
  Close (unit=11)
End Subroutine ReadInputFile

!> Read the specified number of ingredients from input lines
!
Subroutine ReadIngredients (n, lines, ingrs)
  Use Types
  Implicit None
  Integer, Intent(In) :: n
  Type(Ingredient), Dimension (n), Intent(Out) :: ingrs
  Character(Len=256), Dimension (n), Intent(In) :: lines
  Integer :: i, curs, next, propval
  Character (Len=32) :: propname
  Do i = 1, n
    ! Get ingredient name before colon ':'
    curs = Index (lines(i), ':')
    ingrs(i)%ingrname = lines(i) (1:curs-1)
    curs = curs + 1
    ! Read properties
    Do While (curs < len_trim (lines(i)) .And. curs /= 0)
      Do While (lines(i)(curs:curs) == ' ')
        curs = curs + 1
      End Do
      ! Read property name
      next = Index (lines(i)(curs:), ' ')
      If (next > 0)  next = next + curs - 1
      propname = trim (lines (i) (curs:next-1))
      curs = next + 1
      ! Read property value
      Read (lines(i)(curs:), *)  propval
      ! Store property
      If (Trim (propname) == "capacity") Then
        ingrs(i)%capacity = propval
      Else If (Trim (propname) == "durability") Then
        ingrs(i)%durability = propval
      Else If (Trim (propname) == "flavor") Then
        ingrs(i)%flavour = propval
      Else If (Trim (propname) == "texture") Then
        ingrs(i)%textur = propval
      Else If (Trim (propname) == "calories") Then
        ingrs(i)%calories = propval
      Else
        Write (*,'(A,A)') "Error: Unknown ingredient found, ignoring: ", &
          & propname
      End If
      ! Find end of property and start of next
      next = Index (lines(i)(next:), ',')
      If (next > 0) Then
        curs = curs + next - 1
      Else
        curs = 0
      End If
    End Do
  End Do
End Subroutine ReadIngredients

!> Output a recipe
!
Subroutine PrintRecipe (ningr, recipe, ingredients)
  Use Types
  Implicit None
  Integer, Intent(In) :: ningr
  Integer, Dimension(ningr), Intent(InOut) :: recipe
  Type(Ingredient), Dimension(ningr), Intent(In) :: ingredients
  Integer :: i
  Integer :: CookieCalories
  Do i = 1, ningr
    Write (*,'(A,I3,A,A)') "- ", recipe(i), " ", Trim (ingredients(i)%ingrname)
  End Do
  Write (*,'(A,I4)') "-> Calories: ", CookieCalories (ningr, recipe, ingredients)
End Subroutine PrintRecipe

!> Calculate calories of a particular recipe
!
Integer Function CookieCalories (ningr, recipe, ingred)
  Use Types
  Implicit None
  Integer, Intent(In) :: ningr
  Integer, Dimension(ningr), Intent(In) :: recipe
  Type(Ingredient), Dimension(ningr), Intent(In) :: ingred
  Integer :: i, cal
  cal = 0
  Do i = 1, ningr
    cal = cal + recipe (i) * ingred (i) % calories
  End Do
  CookieCalories = cal
End Function CookieCalories

!> Calculate a cookie score for the recipe
!
Integer Function CookieScore (ningr, recipe, ingred, calories)
  Use Types
  Implicit None
  Integer, Intent(In) :: ningr
  Integer, Dimension(ningr), Intent(In) :: recipe
  Integer, Intent(Out) :: calories
  Type(Ingredient), Dimension(ningr), Intent(In) :: ingred
  Integer :: i, cap, dur, flv, tex, cal, score
  cap = 0
  dur = 0
  flv = 0
  tex = 0
  cal = 0
  score = 0
  Do i = 1, ningr
    cap = cap + recipe (i) * ingred (i) % capacity
    dur = dur + recipe (i) * ingred (i) % durability
    flv = flv + recipe (i) * ingred (i) % flavour
    tex = tex + recipe (i) * ingred (i) % textur
    cal = cal + recipe (i) * ingred (i) % calories
  End Do
  If (cap .gt. 0 .And. dur .gt. 0 .And. flv .gt. 0 .And. cal .gt. 0) Then
    CookieScore = cap * dur * flv * tex
  Else
    CookieScore = 0
  End If
  calories = cal
End Function CookieScore

!> Generate the next recipe from the current one by shifting ingredients
!!
!! Take the last ingredient (m) with non-zero amount (a) and move one unit to
!! the next lower ingredient (m-1), the rest to the
!! last ingredient in the list (ningr)
!!
!! Notable cases:
!! - Last ingredient has amount a > 0:  Moving (a-1) to the last ingredient
!!   is moving to itself, i.e. decrementing the last ingredient
!! - Only the first ingredient (m=0) is used:  Moving 1 to the
!!   next lower ingredient (m=-1) is done by incrementing the last ingredient
!!   (ningr).  Together with the rest (a-1) already in the last ingredient,
!!   that ingredient gets the complete amount (a-1+1)
!!
!! Examples:
!!   (0, 2) -> (1, 1) -> (2, 0) -> (0, 2) -> ...
!!   (0, 2, 1) -> (0, 3, 0) -> (1, 0, 2) -> (1, 1, 1) -> (1, 2, 0) -> (2, 0, 1) -> ...
!
Subroutine IncrementRecipe (ningr, recipe)
  Implicit None
  Integer, Intent(In) :: ningr   ! Number of ingredients
  Integer, Dimension(ningr), Intent(InOut) :: recipe
  Integer :: i, n
  Do i = ningr, 1, -1
    If (recipe (i) > 0) Then
      n = recipe (i)   ! Save current value
      recipe (i) = 0   ! Clear this digit
      recipe (ningr) = n - 1   ! Set last digit to decremented value
      ! Increment next higher digit or last digit
      If (i > 1) Then
        recipe (i - 1) = recipe (i - 1) + 1
      Else
        recipe (ningr) = recipe (ningr) + 1
      End If
      Exit
    End If
  End Do
End Subroutine IncrementRecipe

! Try all possible recipes to find the one with the highest cookie score
!
Integer Function FindBestRecipe (ningr, ingredients, maxamount, bestrecipe, &
    & targetcalories)
  Use Types
  Implicit None
  Integer, Intent(In) :: ningr   !< Number of ingredients
  Integer, Intent(In) :: maxamount   !< Total amount of ingredients to add
  !> Select only recipes with this number of calories, 0 for all recipes
  Integer, Intent(In) :: targetcalories
  Type(Ingredient), Dimension(ningr), Intent(In) :: ingredients
  Integer, Dimension(ningr), Intent(Out) :: bestrecipe
  Integer, Dimension(ningr) :: testrecipe
  Integer :: i, n, numrecipes, bestscore, testscore, testcal
  Integer :: CookieScore
  ! Set to recipe with only the last ingredient
  Do i = 1, ningr - 1
    testrecipe (i) = 0
  End Do
  testrecipe (ningr) = maxamount
  bestscore = 0
  testscore = CookieScore (ningr, testrecipe, ingredients, testcal)
  If (testscore .gt. bestscore .And. &
      & (targetcalories .eq. 0 .Or. targetcalories .eq. testcal)) Then
    bestscore = testscore
    bestrecipe(:) = testrecipe(:)
  End If
  numrecipes = 1
  Call IncrementRecipe (ningr, testrecipe)
  ! Increment recipe until every combination was tried
  ! (until the whole amount is in the last ingredient again)
  Do While (testrecipe (ningr) < maxamount)
    ! Better than the last?
    testscore = CookieScore (ningr, testrecipe, ingredients, testcal)
    If (testscore .gt. bestscore .And. &
        & (targetcalories .eq. 0 .Or. targetcalories .eq. testcal)) Then
      bestscore = testscore
      bestrecipe(:) = testrecipe(:)
    End If
    numrecipes = numrecipes + 1
    ! Modify recipe
    Call IncrementRecipe (ningr, testrecipe)
  End Do
  Write (*,'(A,I8)') "Tested recipes: ", numrecipes
  FindBestRecipe = bestscore
End Function FindBestRecipe
