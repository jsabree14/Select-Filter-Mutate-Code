#####GETTING STARTED#####

#Though dplyr contains many functions,
  #its most known for its 5 key "verbs": mutate(), select(), filter(), summarise(), and arrange()
  #In addition to its functions, dplyr relies heavily on the pipe operator, %>%. This is read as
    #"and then." It's use will be briefly explained in the filter() section.
  #This tutorial shows these 5 verbs and others included in the dplyr package.

#First, load dplyr
library(dplyr)

#dplyr also contains preloaded datasets, which we'll be using to show its functionality. Let's load them.
data("starwars")

#A helpful dplyr function is glimpse(). It allows you to see datasets; its base R equivalent is str()
  #Compare both below:
str(starwars)
glimpse(starwars)
#As you can see, glimpse is very neat and will never fill more than your console can currently show.
  #In contrast, str() will fill as much space as it needs. Usually, you should use glimpse() instead of str()

######

####SELECT#####
#select() is pretty straightforward. As its name implies, it allows you to select specific columns to do
  #your analyses on. This is very helpful if you have a huge dataset but only need a few variables.

starwars %>%
  dplyr::select(name, hair_color, skin_color, eye_color)

#The pipe--that is, the %>%--reads as "and then", so we take the starwars dataframe AND THEN select the
  #specified columns. This pipe operator makes it so the programmer does not need to keep typing 
  #starwars$eye_color or starwars$skin_color. 
  #Note: the dplyr:: means that I want to force R to use select from the dplyr package. Unfortunately,
  #other packages also have a function called select, so if you get an error message and your code is
  #correct, it may be because R is using the wrong select. dplyr:: prevents that!

#Select is usually most beneficial when you save your smaller dataframe/tibble into an object:

selected_starwars <- starwars %>%
  dplyr::select(name, hair_color, skin_color, eye_color)
#Now, you can do analyses with this smaller dataset.

#Let's imagine that you want all 13 variables in the starwars dataset EXCEPT for species. You could type
  #them all out, but there are much easier ways to do this. For instance, the - sign before a variable
  #means to NOT select that variable:

starwars %>%
  select(-species)
#notice at the top that this tibble is now 87 rows by 12 columns instead of 13 columns

#If you don't want species and homeworld, you just have to put them in a vector:

starwars %>%
  select(-c(species, homeworld))

#You can also select variables based on their order in your dataset. For instance, except for the first variable name,
  #the second through sixth variables involve physical descriptors of the character (height, eye_color, etc)
  #Let's say that you do not want those descriptors. You have a few options. One is to do what we did above:

starwars %>%
  select(-c(height, mass, hair_color, skin_color, eye_color))

#But an easier way is by the order of the variables:

starwars %>%
  select(-c(height:eye_color))
#This is a great strategy if you're organized about your variable order, for instance, with items on a
  #particular scale or all personality variables in one section. One word of warning is that going off
  #of order can be dangerous if you re-order your variables. Use with caution if you are in the early
  #stages of data organization!

#You can also select based on the variables' names. For instance, ends_with()
  #selects on columns that end with the same string:

starwars %>%
  select(ends_with("color"))

#Similarly, starts_with() picks variables that start with the same string:

starwars %>%
  select(starts_with("h"))

#Last, contains() is the most liberal and selects columns that contain
  #a specific character string anywhere in the name:

starwars %>%
  select(contains("_"))

#You can also use the negative of these commands:

starwars %>%
  select(-ends_with("color"))

#For a similar effect to contains, you can also use matches:
starwars %>%
  select(matches("color"))
#Matches can get a different effect if you have different variables in caps/lowercase (not recommended)
  #by specifying its optional second argument: ignore.case to FALSE

starwars %>%
  select(matches("COLOR", ignore.case = FALSE))
#This yields no matches here because none of the color variables are in caps

#A helpful command if you're dealing with scale data is num_range(). Most
  #scale data is named something like item_1, item_2, etc. Let's imagine that
  #the starwars' colors were organized this way and called color_1, color_2, and
  #color_3 instead of their current names (hair_color, etc).
  #Don't worry about this below renaming code yet--we'll get to it later
num_starwars <- starwars %>%
  rename(color_1 = hair_color,
         color_2 = skin_color,
         color_3 = eye_color)

#Now that the colors are named how scale items should be, you can easily select
  #just those scale items:
num_starwars %>%
  select(num_range("color_", 1:3))
#the first argument of num_range() is the character string to match (i.e.
  #everything that is NOT a number) and the second argument is the numeric
  #range.

#Again, you can combine num_range with other select calls too. For instance,
  #maybe you want the name variable too:

num_starwars %>%
  select(name, num_range("color_", 1:3))

#Most important, you can chain these commands together as well:

starwars %>%
  select(ends_with("color")) %>%
  select(-starts_with("h")) %>%
  select(-skin_color, everything())
#To verbalize, this command takes the starwars dataframe AND THEN selects
  #all columns ending with color AND THEN selects all columns that do NOT
  #start with "h" AND THEN puts skin_color at the end of everything in
  #the dataframe

#Advanced select: select_if:

#You can select variables that only meet a criteria. For instance,
  #You might want to select IF the variable is numeric:

starwars %>%
  select_if(is.numeric)

#You can also select IF a variable does NOT meet a criteria; however,
  #for the most parsimonious solution, you'll need to use the negate() 
  #function from the purrr package.

library(purrr)
starwars %>%
  select_if(negate(is.character))

#Finally, another select helper is one_of(). one_of() is great if you're
  #working with a huge dataset and can't remember the exact name of the 
  #variables. For instance, let's say you accidentally thought there was
  #a teeth_color variable. If you did one_of(), you'd get:
starwars %>%
  select(one_of(c("eye_color", "teeth_color", "skin_color")))
  #A warnning message that there's an unknown column, but it will run.
  #Contrast that with the error message that you'd receive if you did NOT
  #use one_of()
starwars %>%
  select(eye_color, teeth_color, skin_color)
#It still gives an error but does not run.
#Note: select does NOT put variables in quotes, but one_of() does.

##Reorganizing and Renaming Variables##

#You can also use select to rearrange the order of your variables. Let's say
  #you want height to be the *first* variable:

starwars %>%
  select(height, everything())
#everything() tells select to retain every column

#Let's say that you want name to be at the end:
starwars %>%
  select(-name, everything())

starwars %>%
  select(-hair_color) %>%
  select(-height, everything())
#When you have a negative BEFORE everything, it moves that column to the *end*
  #of the dataframe. However, if you have a negative AFTER everything(), R
  #REMOVES the column(s) from the dataframe:
starwars %>%
  select(everything(), -name)
#Note how there's only 12 columns instead of 13

#As we saw earlier, you might want to rename your columns to make them easier to select.
  #This is incredibly easy in R! If you have only a few columns, you can
  #do it manually:

starwars %>%
  rename(new_name = name,
         new_height = height)
#As shown above, the new variable name goes on the left, and the old name goes on the right

#However, if you have a ton of variables. You can automate it! For instance,
  #This code renames variables IF they are numeric by adding "_num" to the end:

starwars %>%
  rename_if(is.numeric, ~paste0(.x, "_num"))
  #paste0() literally tells R to paste these values together. The .x represents the
    #column, and "_num" is the string you're adding. Don't forget the tilde (~) in
    #front of paste0!

#By renaming your variables in this way, you can make much more concise code when it comes
  #to selecting your variables! For instance:

starwars %>%
  rename_if(is.numeric, ~paste0(.x, "_num")) %>%
  select(name, ends_with("_num"))

#You can also rename all variables at once:
starwars %>%
  rename_all(~paste0("new_", .x))
  #Note: by putting the .x after string, the "new_" comes first
    #Another sidenote: it's usually recommended for variables names to start
    #with an alphabetic character, not a number or special character

#Last, and most powerfully, you can rename at specific variables.
  #For instance, maybe you want to rename variables ending in color to hue

#First, you need to load the stringr package that deals with character strings:

library(stringr)
starwars %>%
  rename_at(vars(ends_with("color")), ~str_replace_all(.x, "color", "hue"))
#This code reads as take the starwars dataframe AND THEN rename at the
  #variables (vars) that end with the string "color". For all variables
  # that meet that criteria (.x), replace all instances of that character
  # string (str_replace_all) "color" with "hue"

#Alternatively, you might want to add numbers to the end of these variables.
  #This makes more sense if you forgot to add item numbers to the end:

starwars%>%
  rename_at(vars(ends_with("color")), ~paste0(.x, "_", 1:3))
#Note: 1:3 is NOT in quotes because I want R to count 1, 2, and 3 and put
  #them to in order. If you do this strategy, make sure that you know how
  #many variables this applies to because if you don't, you'll receive an
  #error (for instance, change the above code from 1:3 to 1:4).

#For the above example to work (1:3), you need to know the exact number of
  #columns that this applies to (try changing the above code to 1:4; it 
  #won't run). If you have a big or new dataset, you might not know the
  #exact numbers of columns that meet this specific criteria. Fortunately,
  #there's an easy way to find out:

starwars %>%
  select(ends_with("color")) %>%
  length()
#This code says take the starwars dataframe AND THEN select columns
  #ending with "color" AND THEN report how many there are (length())

#Finally, just like most dplyr functions, you can chain renames together to
  #seamlessly make multiple changes at once:

starwars%>%
  rename_at(vars(ends_with("color")), ~str_replace_all(.x, "color", "hue")) %>%
  rename_at(vars(ends_with("hue")), ~paste0(.x, "_", 1:3))

#####

#####FILTER####
#filter allows you to pick variables that match a certain criteria. 
  #For instance, we may filter the Star Wars characters with yellow eyes

starwars %>%
  filter(eye_color == "yellow")
#As mentioned in the select() section, the pipe--that is, the %>%-- reads as "and then", so we take the starwars dataframe 
  #AND THEN filter for cases in which eye_color is yellow. This pipe operator makes it so the 
  #programmer does not need to keep typing starwars$eye_color or starwars$skin_color. 

#The above code is good if you want to just see how many characters have yellow eyes, but usually
  #we want to do analyses on our filtered data. To do that, you should say your filtered dataset
  #into a new object:

yellow_eyes <- starwars %>%
  filter(eye_color == "yellow")

#Filtering on multiple criteria is possible. | represents OR; thus, to filter on characters with
  #yellow OR red eyes:

starwars %>%
  filter(eye_color == "yellow" | eye_color == "red")

# you use & for AND. This requires the case to match both (or more) criteria:

starwars %>%
  filter(eye_color == "yellow" & skin_color == "red")

#If your data is quantitative, then you can filter with greater and less than criteria too:

starwars %>%
  filter(height <= 167 & mass > 70)

#If you want values between a range, you can type it out:

starwars %>%
  filter(height < 140 & height > 70)

#Or, you can use the between() function to get the same results:

starwars %>%
  filter(between(height, 70, 140))
#Note: always put the smaller value first with between or you will get an empty data frame

#You can also filter for everything that does NOT meet a criteria by using !=

starwars %>%
  filter(eye_color != "yellow")

# You can easily filter out rows with missing data:
starwars %>%
  filter(!is.na(hair_color))

#####

#####ADVANCED FILTERING#####
#there are 3 more advance types of filtering: filter_all(), filter_if(), and filter_at()


###filter_all()###
#filter_all() filters across ALL variables that meet the specified criteria. For instance,
  #if we wanted to see if anyone had a height OR weight below 100, we could filter_all()

starwars %>%
  dplyr::select(name:species) %>%
  filter_all(any_vars(. < 100))
#While this may not make logical sense in the context of weight and height, it would matter more
  #if you wanted to filter anyone that scored above a set score on a series of tests or item
  #To explain the code, it first filters out the 3 variables that are lists, as you need to 
  #use filter_all() with data frames for quantitative values. Then, within filter_all, you need
  #to specific any_vars(), so R knows that this applies to all variables that meet the criteria.
  # the . is shorthand for every possible case, so you are filter all and any variables that
  # has a value less than 100 at some place in the row.

#You can also filter on all variables that have a certain character string
library(stringr) #this package is needed for str_detect(); this function detects character strings
starwars %>%
  filter_all(any_vars(str_detect(., pattern = "yellow")))
  #This code filters all rows that do not have at least column with yellow in it. As above,
  #the . is shorthand for all character variables that may meet the criteria. Thus,
#it matches "yellow" but would also be triggered by the string "grey, green, yellow"
#If you want it to ONLY match yellow and exclude any non-perfect matches, use:

starwars %>%
  filter_all(any_vars(. == "yellow"))

#for smaller datasets, filter_all() may be overkill; however, if a dataset has hundreds of variables,
  #it becomes a powerful function

#You can also use filter_all() to see which rows have any missing data:

rows_with_NAs <- starwars %>%
  filter_all(any_vars(is.na(.)))

#As a side note, you can easily drop any rows with NAs using a function from tidyr:
library(tidyr)
starwars_noNAS <- starwars %>%
  tidyr::drop_na()

###


###filter_if()###
#filter_if() allows you to specify a specific criteria of a column, unlike filter_all(). For instance,
  #you can filter down to any row that is missing any piece of data that is the class integer:

rows_with_intNAs <- starwars %>%
  filter_if(is.integer, any_vars(is.na(.))) 
#This code reads as follows: Take the starwars dataframe AND THEN filter IF ANY columns of class integer
  #are missing any data. (Note: any_vars and the . in is.na will be explained in the filter_at section)
#Note: by default, the starwars dataset reads height in as an integer but mass and birth_year are doubles.
  #Integers must be whole numbers but doubles do not. While this can be changed in R, I kept all results
  #using the dataset's defaults, even if birth_year logically should an integer and height a double.

#To see why you would want filter_if(), we can use another dplyr verb, count(), which counts the number of
  #rows in a tibble/dataframe to compare how many rows were flagged in our filter_if vs our filter all:
count(rows_with_NAs) #made using filter_all()

count(rows_with_intNAs) #made using filter_if()

#Now, imagine you had a dataset in which most of the missing data was in a character column that wasn't
  #needed for your statistical analyses. filter_all() would pull many unneeded rows, whereas filter_if()
  #will allow you to be more percise. Here, the difference was only 52 rows, but imagine if it were in
  #the hundreds or thousands.

#As with most dplyr verbs, you can chain them together. The first two lines of code are what you've already
  #seen. On the third line, you're taking what you've already filtered and filtering it further:
  #if any of the character columns contain the string "brown", then you want them.
starwars %>%
  filter_if(is.integer, any_vars(is.na(.))) %>%
  filter_if(is.character, any_vars(. == "brown"))

#Long story short, using a filter_if saves you from having to type each row out!

###

###filter_at###
#Last, filter_at() allows you to specify specific columns that you want to filter at. For instance, 
  #just as we did in the above example, maybe you only care about the color of the character. Thus,
  #you can filter at only the color variables:

starwars %>%
  filter_at(vars(contains("color")), any_vars(. == "light"))
#The first argument is to specify the variables that you want. Even if you want to write each variable
  #out, you still need to put them in the var() wrapper. For example, you get the same answer with:

starwars %>%
  filter_at(vars(hair_color, skin_color, eye_color), any_vars(. == "light"))
#The second argument is the quality to filter on. It either has to be wrapped in any_vars() or all_vars()
  #any_vars() is akin to an OR statement: only one variable needs to be TRUE. In contrast, all_vars() is
  #akin to an AND statement and each specified column must be true. Let's compare them with the color
  #"brown"

starwars %>%
  filter_at(vars(contains("color")), any_vars(. == "brown"))

starwars %>%
  filter_at(vars(contains("color")), all_vars(. == "brown"))

#within the call to any/all_vars() is first a . which is shorthand for all variables and then the second
  #portion is whatever you want to filter on and is written like a regular filter command.

#In short, filter_at allows you to be even more percise than filter_if()

###

star <- starwars %>%
  filter_at(vars(contains("color")), any_vars(. == "black")) %>%
  dplyr::filter(gender == "male") %>%
  dplyr::filter(height < 175)

light <- starwars %>%
  dplyr::filter(gender == "female") %>%
  dplyr::filter(eye_color != "brown") %>%
  filter_at(vars(contains("color")), any_vars(. == "black"))

sibley <- starwars %>%
  filter_if(vars(gender) == "female")

#doesn't have brown eyes and has black somewhere and is a woman

#Want more information on advanced filtering? Then go to this very helpful tutorial, which
  #was instrumental in writing this section: https://suzan.rbind.io/2018/02/dplyr-tutorial-3/  
#####

####MUTATE####

#Again, we'll be working with the starwars dataframe.

library(dplyr)
data("starwars")

#As explained in the select() tutorial, you can view the data with glimpse()
glimpse(starwars)

#For demonstration purposes, we're only going to use a subset of the starwars
  #dataset. If you want more information on how to select specific variables,
  #please see the select() tutorial.

small_star <- starwars %>%
  select(name, height, mass, gender, birth_year)

glimpse(small_star)

#mutate allows you to create new variables in your dataset. For instance, maybe you want to a new
  #variable called density, which we'll define as the characters' mass divided by their height:

small_star %>%
  mutate(density = mass/height)
#Note: You read this code as "Take the small_star dataframe AND THEN 
  #mutate it to create a new variable called density, which is each 
  #row's mass divided by its respective height."

#You can see that R calculated the density correctly! 

#You can also mutate multiple variables at once by separating them
  #with a comma:

small_star %>%
  mutate(density = mass/height,
         silly_var = mass+height)

#Let's say you want to keep density permanently. You can make a new 
  #object with density, or you can save it in the same small_star dataframe:

small_star <- small_star %>%
  mutate(density = mass/height)

#Now, small_star has 6 variables

#Before we do more mutate examples, arrange() allows you to sort your data by a certain value.
  #By default, arrange sorts in ascending order. To sort in ascending density, use arrange(density):

small_star %>%
  arrange(density)
#Again, to permanently save the small_star in this new order, you'd need
  #to reassign it to small_star with small_star <- ....

#If you want to sort in descending order, you just need to do arrange(desc(density))

small_star %>%
  arrange(desc(density))

#You can also group variables together, such as the average mass by gender. This
  #is the same as doing a "split file" in SPSS. To do so, use group_by()

small_star %>%
  group_by(gender) %>%
  mutate(gen_mass = mean(mass, na.rm = TRUE)) %>%
  ungroup()

#This code reads as take the small_star dataframe AND THEN group by gender
  #AND THEN mutate the dataframe by adding a new variable called gen_mass,
  #which is the mean mass (by gender), and remove (rm) all missing values (nas)
  #AND THEN stop grouping the variables by gender (ungroup)
#Note: In this example, you do not *need* to ungroup, but it is good
  #practice to. That way, you do not accidentally run group analyses 
  #when you wanted individual ones.

#When you group variables, you can also arrange them by group by setting the optional
  # .by_group argument to TRUE:

small_star %>%
  group_by(gender) %>%
  mutate(gen_mass = mean(mass, na.rm = TRUE)) %>%
  arrange(.by_group = TRUE)

#Grouping and ungrouping is powerful because you can chain them together
  #To do helpful analyses, such as centering. For instance, let's say
  #you want to center (subtract the mean) each character's mass based
  #on their gender:

small_star %>%
  group_by(gender) %>%
  mutate(gen_mass = mean(mass, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mass_CentbyGen = mass - gen_mass)

#If you want to group by multiple variables, you can list them out, such as with
  #group_by(eye_color, hair_color, skin_color) or you can use group_by_at(), and then
  #you can use the select helpers, like contains():

color_mass <- starwars %>%
  group_by_at(vars(contains("color"))) %>%
  mutate(gen_mass = mean(mass, na.rm = TRUE)) %>%
  ungroup()

#Let's check the above output using another dplyr function: distinct
color_mass %>%
  distinct(gen_mass)
#distinct() tells you how many unique values are in a column or across columns
#As an aside, you can see the unique combinations of columns there
  #are. For instance:

color_mass %>%
  distinct(gen_mass, height)

#Alternatively, if you just want to see how many unique combinations 
  #that there are, you can use n_distinct:

n_distinct(color_mass$gen_mass, color_mass$height)
  #Note: for n_distinct, you need to use the dataframe$variable format

#Sometimes, you may have numeric data, like mass, that you want to convert
  #into categorical data/factors. There are many options, but if_else()
  #and case_when() streamline this process.

#For basic cases with only two options, if_else() will suffice. Let's 
  #say if a character is less than the median height, s/he is "short".
  #Else, s/he is "tall". 

height_ex <- small_star %>%
  mutate(height_cat = if_else(height >= median(height, na.rm = T), "tall", "short"))
#This code reads as take the small_star dataset AND THEN mutate it by creating a 
  #new variable called height_cat. If height is greater than or equal to the median
  #height in the data (167 here), then label the character as "short", else,
  #label him/her as "tall".
#if_else() has 3 required arguments and one optional one. The first is the
  #logical condition (height >= median(height, na.rm = T)). The second is the
  #response if the argument is TRUE ("tall"), and the third is if the 
  #condition is FALSE ("short"). The optional argument is what you want R to say
  #if it finds a missing value. The default is to leave it as NA 

#When you have more than 2 conditions, you'll want to use case_when(). For 
  #instance, let's say with mass that we want multiple categories (light,
  # average, huge).

small_star <- small_star %>%
  mutate(mass_cat = case_when(
    mass < 56 ~ "Light",
    mass >= 56 & mass < 84 ~ "Average",
    mass >= 84 ~ "Huge",
    TRUE ~ "NA"))
#Read the code as: take the small_star dataframe AND THEN mutate it by creating a
  #new column called mass_cat. When mass is less than 56, label it "light"
  #when mass is between 56 and 84, label it "average", when mass is greater
  #than or equal to 84, label it "huge", and if any other condition is TRUE,
  #label it NA.

#case_when() is powerful because you can use it *across* variables too:

small_star <- small_star %>%
  mutate(dense_cat = case_when(
    mass < 56 & height >= 191 ~ "Lanky",
    mass >= 56 & mass < 84 & height >= 167 & height < 191 ~ "Average",
    mass >= 84 & height > 167 ~ "Stout",
    mass == NA | height == NA ~ "NA",
    TRUE ~ "You get the gist of it"
  ))

head(small_star$dense_cat)

#You can also have case_when pull values from other columns. All the values must be
  #characters, however. If you want it to pull a numeric value, you can easily work
  #around this by wrapping the value in the as.character() function, which tells R to
  #read the value as a character string instead of a number. Then, when you have all your
  #values read in, you can wrap the new column in a as.numeric() wrapper!
small_star <- small_star %>%
  mutate(dense_cat2 = case_when(
    mass < 56 & height >= 191 ~ as.character(birth_year),
    mass >= 56 & mass < 84 & height >= 167 & height < 191 ~ as.character(height),
    mass >= 84 & height > 167 ~ as.character(mass),
    mass == NA | height == NA ~ "NA",
    TRUE ~ "NA"),
  dense_cat2 = as.numeric(dense_cat2))

#Going back to group_by(), you can also group by multiple groups at once:

small_star %>%
  group_by(gender, mass_cat) %>%
  mutate(birth_gen_mass = mean(birth_year, na.rm = TRUE))%>%
  ungroup() %>%
  select(birth_gen_mass)

#####

small_star %>%
  group_by(gender, mass_cat) %>%
  summarize(age = mean(birth_year, na.rm = TRUE))

small_star %>%
  group_by(gender, mass_cat) %>%
  summarize(age = mean(birth_year, na.rm = TRUE),
            n = n())

star_summary <- small_star %>%
  group_by(gender, mass_cat) %>%
  summarize(age = mean(birth_year, na.rm = TRUE),
            n = n())

#n() can be used within filter, mutate, and summarize

#As an aside, sometimes you may *only* want the results from your mutated column,
  #and no longer want the other rows. This can be done easily by substituting 
  #mutate() with transmute(). All transumate syntax is the same as its mutate
  #command. To compare:

small_star %>%
  mutate(density = mass/height)

small_star %>%
  transmute(density = mass/height)

#As may be expected, you can also use transmute_if() and transmute_at():

starwars %>%
  mutate_if(is.character, as.factor)

starwars %>%
  transmute_if(is.character, as.factor)
#In the above example, note how only the modified columns are retained

starwars %>% 
  mutate_at(vars(height:mass), mean)

starwars %>% 
  transmute_at(vars(height:mass), log10)

#Back to mutating the starwars dataset. You can also use functions in your mutations. While its a 
#little silly in this example, let's say that you wanted to find the sum between a character's 
#height and mass (i.e. 100 and 50 summed is 150).

starwars %>%
  mutate(height_birth = select(., height, birth_year) %>% 
           rowSums(na.rm = TRUE),
         mass_height = select(., mass, height) %>%
           rowSums(na.rm = T)) %>%
  select(name, height_birth, mass_height, mass, height, birth_year)

#Bonus: Let's say you want to center multiple variables at once in multiple ways,
#(i.e., within person and grand mean), you can do so with this code:

mlm <- tibble(id = c(1, 1, 1, 2, 2, 2, 3, 3),
              score = c(0, 2, 3, 6, 7, 8, 10, 20),
              grade = c(80, 90, 100, 70, 70, 70, 100, 70))

mlm %>%
  mutate_at(vars(score, grade), .funs = list(grand_cent = ~. - mean(., na.rm = T))) %>%
  group_by(id) %>%
  mutate_at(vars(score, grade), .funs = list(cent_id = ~. - mean(., na.rm = T))) %>%
  ungroup()