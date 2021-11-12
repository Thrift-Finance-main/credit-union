# project-sahara
A Croud Stake Group built on the Cardano block-chain using the Plutus smart-contract programming language.

[Crowd Staking Requirements](https://docs.google.com/document/d/1jjm7pPme6_3yPaTw1BKobds4Mf3WEACPGW4R10c_laI/edit)


## Getting Started

### Plutus Playground

The [src code](./app/code/Main.hs) can be posted directly into [Plutus Playground](https://playground.plutus.iohkdev.io/)

### Set up the Plutus Core Libraries Using Nix
Instruction on how to do that can be found [here](https://www.reddit.com/r/cardano/comments/mmzut6/macos_plutus_playground_build_instructions/) (although these are not the [official instructions)[https://github.com/input-output-hk/plutus#nix-advice] they are the most straight forward.

* clone this project: ```git clone https://github.com/Thrift-Finance-main/project-sahara.git ```
* change directory to the plutus project root and execute ```nix-shell```
* once the nix-shell has started ```cd project-sahara; cabal build```





## Design
Using crowdfunding code as a base , campaign actions are Stake and Refund

A Campaign Owner opens a campanin with a startdate , enddate and maxStakers and the wallet address of a StakePool

A Contributer can either Stake or Request a Refund
###Staking
A Contributor can only stake if the maximum number of Stakers isnt reached and it is before the campaign start time
A Stake must have an amount and a beneficiary Address for ROA

###Refund
A Refund can happen any time before the campaign start, once the campaign starts a refund cannot be issued until the campaign is ended
A Refund consists of returning all staked ada to the original stakers. This will always run
###Starting a Campaign
Once a campaign is started by the campaign owner the campaign will wait until the campaign has start date has begun
It will then use all of send all of the ada it has collected to the staking wallet
At intervals the campaign will withdraw the ROA (Return on Ada) of the stake wallet and award it to the beneficiary stated by a contributor
Once all campaigner owners are paid ROA the contract issues refunds for all contributors and refunds all stakes.
