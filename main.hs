module Main where

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import ToDoListManager
    ( addTask, deleteTask, editTask, emptyTaskList, isTaskExist, markFinished, taskFinished, showTask, ToDoListManager )
import Data.Char (toLower)

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
    putStrLn "3. Edit Task"
    putStrLn "4. Mark Task as Finished"
    putStrLn "5. Show Task List" 
    putStrLn "6. Exit"

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
            if null currentTaskList 
                then do
                    putStrLn "No tasks have been added yet!"
                    mainLoop currentTaskList
                else do
                    idStr <- prompt "Enter the ID of the task you want to delete : "
                           
                    case readInt idStr of
                        Just targetTaskId -> do
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
            if null currentTaskList 
                then do
                    putStrLn "No tasks have been added yet!"
                    mainLoop currentTaskList
                else do
                    idStr <- prompt "enter the ID of the task you want to edit: "

                    case readInt idStr of
                        Just targetTaskId ->
                            if not (isTaskExist targetTaskId currentTaskList)
                                then do
                                    putStrLn "Task not available!"
                                    mainLoop currentTaskList
                                else do
                                    newDesc <- prompt "Enter new description of the task: "
                            
                                    if null newDesc
                                        then do
                                            putStrLn "Cannot add empty task description!"
                                            mainLoop currentTaskList
                                        else do
                                            let updatedList = editTask targetTaskId newDesc currentTaskList
                                            putStrLn "Task successfully updated"
                                            mainLoop updatedList
                        Nothing -> do
                            putStrLn "Invalid ID!"
                            mainLoop currentTaskList
        
        
        "4" -> do
            if null currentTaskList
                then do
                    putStrLn "No Task have been added yet!"
                    mainLoop currentTaskList
                else do
                    idStr <- prompt "Enter the ID of the task you want to mark as finished: "

                    case readInt idStr of
                        Just targetTaskId ->
                            if not (isTaskExist targetTaskId currentTaskList)
                                then do
                                    putStrLn "Task not Available"
                                    mainLoop currentTaskList
                                else do
                                    if taskFinished targetTaskId currentTaskList
                                        then do
                                            putStrLn "Task already mark as finished!"
                                            mainLoop currentTaskList
                                        else do
                                            confirmFinished  <- prompt "Mark the task as finished? [y/n] : "

                                            case map toLower confirmFinished of
                                                "y" -> do
                                                    let updatedList = markFinished targetTaskId currentTaskList
                                                    putStrLn "Task successfully mark as finished!"
                                                    mainLoop updatedList
                                                
                                                "n" -> do
                                                    putStrLn "Operation cancelled"
                                                    mainLoop currentTaskList
                                                
                                                _ -> do
                                                    putStrLn "Please enter y or n"
                                                    mainLoop currentTaskList
                        Nothing -> do
                            putStrLn "Invalid ID!"
                            mainLoop currentTaskList
                                                    
        "5" -> do
            putStrLn "Task List"
            putStrLn (showTask currentTaskList)
            mainLoop currentTaskList

        "6" -> putStrLn "See you!"

        _ -> do
            putStrLn "Invalid option!"
            mainLoop currentTaskList

main :: IO ()
main = mainLoop emptyTaskList