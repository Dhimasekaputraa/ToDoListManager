module Main where

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import ToDoListManager
    ( addTask, deleteTask, emptyTaskList, showTask, ToDoListManager )

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

readInt ::String -> Maybe Int
readInt = readMaybe

mainLoop :: ToDoListManager -> IO ()
mainLoop currentTaskList = do
    putStrLn "\n=== TO-DO LIST MANAGER ==="
    putStrLn "1. Add Task"
    putStrLn "2. Delete Task"
    putStrLn "3. Show Task List" 
    putStrLn "4. Exit"

    option <- prompt "Choose menu : "
    case option of
        "1" -> do
            desc <- prompt "Add new task to your list : "
            if null desc
                then putStrLn "Cannot add task!" >> mainLoop currentTaskList
                else do
                    let updatedList = addTask desc currentTaskList
                    putStrLn "Task successfully added!"
                    mainLoop updatedList

        "2" -> do
            idStr <- prompt "Enter the ID of the task you want to delete : "
            case readInt idStr of
                Just targetTaskId -> do
                    if null currentTaskList 
                        then do
                            putStrLn "No tasks have been added yet!"
                            mainLoop currentTaskList
                        else do
                           case deleteTask targetTaskId currentTaskList of
                            Just updatedList -> do
                                putStrLn "Task successfully deleted!"
                                mainLoop updatedList
                            Nothing -> do
                                putStrLn "Task not available!"
                                mainLoop currentTaskList
                Nothing -> do
                    putStrLn "Invalid ID!"
                    mainLoop currentTaskList
        
        "3" -> do
            putStrLn "Task List"
            putStrLn (showTask currentTaskList)
            mainLoop currentTaskList

        "4" -> putStrLn "See you!"

        _ -> do
            putStrLn "Invalid option!"
            mainLoop currentTaskList

main :: IO ()
main = mainLoop emptyTaskList

        

