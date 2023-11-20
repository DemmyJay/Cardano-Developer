module Haskell where

{- 


---------------- importing Libraries into REPL ---------------

-- While using the repl to test your code, to can import libraries into your repl environment by using "import <Library name>" or ":m <+Library name>" where the library name must begins with a puls symbol.
-- To switch Libraries i.e exiting the current library and importing a new one, run ":m <Library name>" -- Library name doesn't begin with any symbol.
-- To exit an imported library, run ":m <-Libraries name>" e.g > :m -Data.list where the Library name must begin with a minus symbol.

------------------------------------ RULES FOR IMPORTING MODULES INTO A HASKELL SCRIPT -----------------------------------------

To import a Haskell module into another one, you need to use the import statement. There are a few conditions and considerations for importing modules in Haskell:

Module Name: 
You need to specify the name of the module you want to import. The module name should match the module's name or be an alias defined in your project. For example, to import the Data.List module, you can use import Data.List or give it an alias like import Data.List as List.


Module Visibility: 
By default, all functions and types from the imported module are visible in the importing module. If you want to import only specific functions or types, you can list them explicitly in parentheses. For example, import Data.List (sort, filter) imports only the sort and filter functions from the Data.List module.


Qualified Imports: 
You can import modules in a qualified manner by adding the qualified keyword. This means you need to prefix all functions and types from the imported module with the module name or an alias. For example, import qualified Data.Map as Map imports the Data.Map module, and you need to use Map.functionName to access its functions.


Hiding Functions and Types: 
If you want to import all functions and types from a module except for specific ones, you can use the hiding clause. For example, import Data.List hiding (head) imports all functions and types from Data.List except for head.


Importing Your Own Modules: 
If you want to import a module you've defined in your project, use the module's name and make sure the module file is in the same directory or a directory listed in your GHC project's search path.


Circular Dependencies: 
Be cautious of circular dependencies, where module A imports module B and vice versa. This can lead to compilation issues. Haskell allows mutually recursive functions and types, but circular module dependencies can be problematic.


Export Lists: 
When defining a module, you can specify which functions and types should be visible to other modules by using an export list in the module header. For example, module MyModule (someFunction, someType) where only exports someFunction and someType from MyModule.


Module Search Path: 
Ensure that your module is in a location that is included in the module search path. GHC looks in the current directory, system libraries, and directories specified with the -i option for modules to import.

By following these conditions and considerations, you can effectively import modules into your Haskell code, manage their visibility, and avoid common issues related to module dependencies.


Package Dependencies: 
If you are using third-party libraries or packages, you may need to specify these dependencies in your project's .cabal file or use a package manager like stack or Cabal to manage and install the required packages.


Qualified Imports with Aliases: 
You can use aliases when importing modules with the qualified keyword. For example, import qualified Data.Map as M allows you to use M.functionName instead of the full module name.


Hidden Instances: 
Be aware that importing a module might bring instances (type class implementations) into scope. Be cautious if you import a module that provides instances that conflict with instances defined elsewhere in your code.


Import Order: 
The order of import statements in your code doesn't usually matter, but it's a good practice to keep them organized for readability.


Overlapping Definitions: 
If you import multiple modules with overlapping function or type names, you may encounter name clashes. You can use qualified imports or explicitly specify which function or type you want to use in such cases.


Re-Exporting Modules: 
In some cases, you might want to import a module and then re-export some or all of its functionality from another module that you define. This is often done when creating high-level interfaces or aggregating related functionality.


Extension Modules: 
Some modules provide language extensions or additional features. These modules may require specific compiler flags or language pragmas to be enabled in your code.

Remember that good module organization and a clear understanding of module dependencies are essential for writing maintainable Haskell code. Properly handling module imports and exports will make your code more readable and manageable as your project grows.


Module Hierarchies: 
Haskell modules can be organized into hierarchies. For example, the Data.List module is a sub-module of the Data module. You can use dot notation to import a specific sub-module, like import Data.List or import Data.List.Submodule.


Qualified vs. Unqualified Imports: 
You can use both qualified and unqualified imports in the same module. This can be useful when you want to access some functions directly and others with a prefix.


Importing Type Classes: 
You can import type classes in the same way you import modules. For example, import Data.Monoid brings the Monoid type class into scope. You can use this to define instances of type classes for your custom data types.


Avoiding Ambiguities: 
If you have multiple imports that introduce conflicting names, you might need to use the as keyword to create unique aliases for specific imports.


Importing Multiple Modules in One Statement: 
You can import multiple modules in a single import statement. For example, import Data.List (sort, filter) qualified Data.Map as Map allows you to import sort and filter from Data.List and the entire Data.Map module with a qualified alias.


Cyclic Dependencies: 
Be cautious when dealing with cyclic dependencies between modules. It can lead to issues and should be avoided or carefully managed.


Minimizing Imports: 
It's a good practice to import only the specific functions and types you need, rather than importing an entire module. This keeps your code more efficient and minimizes potential naming conflicts.


Haskell Language Extensions: 
Some language features or extensions might require specific language pragmas to be enabled with the LANGUAGE or OPTIONS pragma in your code.


Documentation: 
Haskell modules often include documentation comments to describe the purpose and usage of the functions and types they export. These can be accessed with tools like Haddock.

By understanding and following these considerations, you can effectively manage module imports and ensure your Haskell code is well-organized and maintainable.


GHCi and REPL: 
When working in GHCi (the GHC interactive environment), you can import modules using :m or :module. For example, you can type :m + Data.List to load the Data.List module into the current GHCi session. This is useful for experimenting with code interactively.


Module Naming Conventions: 
has naming conventions for modules. Module names should start with a capital letter, and the module name should match the file name (with the extension .hs). For example, a module named MyModule should be defined in a file named MyModule.hs.


Qualified Imports for Clarity: 
Qualified imports are useful for avoiding name clashes, especially when different modules provide functions with the same name. Using qualified imports makes it clear which module the function is coming from.


Documentation: 
You can generate documentation for your Haskell code using tools like Haddock. Proper module and function documentation can greatly improve code maintainability and readability for both you and other developers.


Custom Modules: 
You can create your own modules to organize and structure your code effectively. This is particularly useful as your codebase grows.


Compiler Directives: 
Some compiler directives or flags may influence how modules are imported or compiled. For example, using the -XNoImplicitPrelude flag allows you to have more control over what gets imported by default.


Project Structure: 
For larger Haskell projects, it's common to have a well-structured project directory with a clear organization of modules. Build tools like Cabal or Stack can help manage the project structure.


Scoped Type Variables: 
When using GHC extensions like ScopedTypeVariables, type variables may need to be explicitly quantified in the module's header.


Using Exports: 
You can specify what functions and types you want to export from your module in the module's header. This helps control the interface of your module.

Understanding these additional details will help you effectively manage module imports and exports in your Haskell projects, leading to more organized and maintainable code.



Haskell Platform and Libraries: 
The Haskell Platform includes a set of commonly used libraries and modules. You can import modules from the Haskell Platform or install additional packages from Hackage, a central package repository for Haskell.


Qualified Imports for Clarity: 
Qualified imports can enhance code clarity by explicitly showing the source of a function or type. This is especially helpful when dealing with modules that provide functions with similar names.


Type Re-exports: 
When defining your own modules, you can re-export types and functions from other modules to create a clean and consistent interface. This allows you to present a more focused view of functionality to users of your module.


Language Pragmas: 
Language pragmas such as {-# LANGUAGE ... #-} may need to be included in your source code to enable certain language extensions or to modify how Haskell features are interpreted by the compiler.


Using import ... hiding: 
You can use the hiding clause when importing modules to exclude specific functions or types. For example, import Data.List hiding (sort) imports all functions from Data.List except sort.

Now, let's revisit the topic to see if there's more information to add:

Ambiguity Resolution: 
If there's a naming conflict between functions or types from different modules, you can use the as keyword to create unique aliases for specific imports, helping to resolve the ambiguity.


Qualified Imports in Functions: 
You can use qualified imports in functions to minimize naming conflicts. For example, you can import a module with a qualified alias and use that alias within a specific function to avoid conflicts in the rest of the module.


Please let me know if you have more specific questions or if there's a particular aspect of Haskell modules or imports you'd like to explore further.


-}

--graham hutton: programming in haskell;
-- What is functional Programming:
-- Functional programming is a style in programming where the basic methods of computation is the application of functions to arguments

--Example: 
--summing the integers 1 to 10 in Haskell;
--sum[1..10]
--the basic computation here is function application.
  
--introduction to Haskell:

--List:
--Square brackets [] have to do with lists. ie. A list of integers: [1,2,3,4,5]. empty list: []. 
--Pattern: 
-- *Operators & Symbols: ++, :, |,


i = [6,4,2,8,3]

f :: Ord a => [a] -> [a]
f []     = []
f (x:xs) = f ys ++ [x] ++ f zs
           where
           ys = [a | a <- xs, a <= x]
           zs = [b | b <- xs, b > x]



--Solution:
-- f [3,1,4,2] = f [1,2] ++ [3] ++ f [4]
--             = f [] ++ [1] ++ f [2] ++ [3] ++ [] ++ [4] ++ []
--             = [] ++ [1] ++ [] ++ [2] ++ [3] ++ [4]
--             = [1,2,3,4]


{-
----------------- PROBLEM SOLVING WITH HASKELL --------------------------

1. Analyze the problem.
2. Divide and Conquer: Divide in smaller problems if possible
3. Consider the types
4. Consider the process (the evaluation process..)
5. Consider the identities and base-cases
6. Consider the inputs
7. Code your functions

-} 



-- f x = x+2