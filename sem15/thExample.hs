{-# LANGUAGE TemplateHaskell #-}

module TemplateHaskellExample where

-- Это отдельный файл для примера, потому что импорт ниже сломает предыдущий(

import Control.Lens

-- TODO: Template Haskell, показать этот квирк с интерпретатором

-- Посмотрим, как теперь можно изготавливать оптику с помощью Template Haskell.
-- Пусть у нас есть определение для копроизведения трех типов

data EitherOrOr a b c =
  Lft { _lft :: a } |
  Mid { _mid :: b } |
  Rgt { _rgt :: c } deriving (Eq, Show)

-- Вот этот очень странный вызов сделает нам линзы для нашего типа
-- Здесь странно все. Мы не привязываемся к какому-либо идентификатору
-- Здесь какие-то непонятные кавычки перед именем типа
-- Что все это означает?!

makeLenses ''EitherOrOr
makePrisms ''EitherOrOr
