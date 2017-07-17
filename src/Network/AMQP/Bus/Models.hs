module Network.AMQP.Bus.Models where

import Data.Map (Map, fromList)

data Queue = Queue
  { queueName :: String
  , queueDurable :: Bool
  , queueExclusive :: Bool
  , queueAutoDelete :: Bool
  , queueArguments :: Map String String
  }

data ExchangeType
  = Topic
  | Fanout
  | Direct
  | Headers

data Exchange = Exchange
  { exchangeName :: String
  , exchangeType :: ExchangeType
  , exchangeDurable :: Bool
  , exchangeAutoDelete :: Bool
  , exchangeArguments :: Map String String
  }

data Binding = Binding
  { bindingExchangeName :: String
  , bindingQueueName :: String
  , bindingRoutingKey :: Maybe String
  , bindingArguments :: Maybe (Map String String)
  }

data ConsumerProps = ConsumerProps
  { cpropsPrefetchCount :: Int
  , cpropsNoAck :: Bool
  , cpropsNoLocal :: Bool
  , cpropsExclusive :: Bool
  , cpropsRequeueOnProcessingFailure :: Bool
  , cpropsArguments :: Map String String
  , cpropsTag :: String
  }

defConsumerProps :: ConsumerProps
defConsumerProps =
  ConsumerProps
  { cpropsPrefetchCount = 10
  , cpropsNoAck = False
  , cpropsNoLocal = True
  , cpropsExclusive = False
  , cpropsRequeueOnProcessingFailure = False
  , cpropsArguments = fromList []
  , cpropsTag = ""
  }

data Message = Message
  { msgExchangeName :: Maybe String
  , msgRoutingKey :: Maybe String
  , msgHeaders :: Maybe (Map String String)
  , msgBody :: String
  , msgConsumerTag :: Maybe String
  , msgRedelivered :: Maybe Bool
  , msgMandatory :: Maybe Bool
  , msgPersistent :: Maybe Bool
  , msgCorrelationId :: Maybe String
  , msgContentEncoding :: Maybe String
  , msgContentType :: Maybe String
  , msgType :: Maybe String
  , msgApplicationId :: Maybe String
  , msgUserId :: Maybe String
  , msgId :: Maybe String
  , msgExpiration :: Maybe String
  , msgReplyTo :: Maybe String
  }

defMessage :: Message
defMessage
  = Message
  { msgExchangeName = Nothing
  , msgRoutingKey = Nothing
  , msgHeaders = Nothing
  , msgBody = ""
  , msgConsumerTag = Nothing
  , msgRedelivered = Nothing
  , msgMandatory = Nothing
  , msgPersistent = Nothing
  , msgCorrelationId = Nothing
  , msgContentEncoding = Nothing
  , msgContentType = Nothing
  , msgType = Nothing
  , msgApplicationId = Nothing
  , msgUserId = Nothing
  , msgId = Nothing
  , msgExpiration = Nothing
  , msgReplyTo = Nothing
  }
