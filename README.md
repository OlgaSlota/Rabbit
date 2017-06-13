# RabbitMQ
Haskell implementation of basic communication mechanism with AMQP

## A short how to :

### Running

#### example :
2 clients :

stack requestCalculation.hs logic 0 1

stack requestCalculation.hs algebra 0 3

server :

stack processCalculation.hs logic algebra

### Testing
stack test              - run all

stack test :unit-tests  - unit tests only
