module ToDoListManager (
    Task(..),
    ToDoListManager,
    emptyTaskList,
    addTask,
    deleteTask,
    showTask
) where

import Data.List (intercalate)

data Task = Task{
    taskId :: Int,
    taskDesc :: String,
    isTaskCompleted :: Bool
} deriving (Show, Eq)

type ToDoListManager = [Task]

emptyTaskList :: ToDoListManager
emptyTaskList = []

addTask :: String -> ToDoListManager -> ToDoListManager
addTask desc currentTaskList =
    let nextId = if null currentTaskList then 1 else maximum (map taskId currentTaskList) + 1
        newTask = Task {taskId = nextId, taskDesc = desc, isTaskCompleted = False}
    in currentTaskList ++ [newTask]

deleteTask :: Int -> ToDoListManager -> ToDoListManager
deleteTask targetTaskId currentTaskList =
    filter (\t -> taskId t /= targetTaskId) currentTaskList
    

showTask :: ToDoListManager -> String
showTask [] = "Belum ada tugas yang ditambahkan"
showTask list = 
    let tableHeader = "-----------------------------------\nID  | Description | Status\n-----------------------------------"
        renderStatus t = if isTaskCompleted t then "Finished" else "Not Finished Yet"
        renderRow t = show (taskId t) ++ "   | " ++ taskDesc t ++ "  | " ++ renderStatus t 
        rows = map renderRow list
        tableFooter = "-----------------------------------"
    in intercalate "\n" (tableHeader : rows ++ [tableFooter])