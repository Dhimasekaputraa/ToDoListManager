module ToDoListManager (
    Task(..),
    ToDoListManager,
    emptyTaskList,
    isTaskExist,
    addTask,
    deleteTask,
    editTask,
    markFinished,
    taskFinished,
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

isTaskExist :: Int -> ToDoListManager -> Bool
isTaskExist targetTaskId currentTaskList =
    any (\t -> taskId t == targetTaskId) currentTaskList

taskFinished :: Int -> ToDoListManager -> Bool
taskFinished targetTaskId currentTaskList =
    any (\t -> taskId t == targetTaskId && isTaskCompleted t) currentTaskList

addTask :: String -> ToDoListManager -> ToDoListManager
addTask desc currentTaskList =
    let nextId = if null currentTaskList then 1 else maximum (map taskId currentTaskList) + 1
        newTask = Task {taskId = nextId, taskDesc = desc, isTaskCompleted = False}
    in currentTaskList ++ [newTask]

deleteTask :: Int -> ToDoListManager -> Maybe ToDoListManager
deleteTask targetTaskId currentTaskList =
    if isTaskExist targetTaskId currentTaskList
        then Just (filter (\t -> taskId t /= targetTaskId) currentTaskList)
        else Nothing

showTask :: ToDoListManager -> String
showTask [] = "No tasks have been added yet!"
showTask list = 
    let tableHeader = "-----------------------------------\nID  | Description | Status\n-----------------------------------"
        renderStatus t = if isTaskCompleted t then "Finished" else "Not Finished Yet"
        renderRow t = show (taskId t) ++ "   | " ++ taskDesc t ++ "  | " ++ renderStatus t 
        rows = map renderRow list
        tableFooter = "-----------------------------------"
    in intercalate "\n" (tableHeader : rows ++ [tableFooter])

editTask :: Int -> String -> ToDoListManager -> ToDoListManager
editTask targetTaskId newDesc =
        map edit
    where
        edit t
            | taskId t == targetTaskId =
                t {taskDesc = newDesc}
            | otherwise = t

markFinished :: Int -> ToDoListManager -> ToDoListManager
markFinished targetTaskId = 
        map mark
    where
        mark t
            | taskId t == targetTaskId =
                t {isTaskCompleted = True}
            | otherwise = t