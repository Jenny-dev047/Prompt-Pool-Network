# PromptPool Network

A decentralized platform for proposing, voting on, and funding AI prompt ideas and generative challenge competitions on Stacks blockchain.

## Features

- Create prompt proposals with funding goals
- Community voting on prompt ideas
- Direct funding through STX contributions
- Transparent tracking of votes and contributions

## Smart Contract Functions

### Public Functions

- `create-prompt` - Submit a new prompt idea with title, description, and funding goal
- `vote-prompt` - Vote for a prompt proposal (one vote per user)
- `fund-prompt` - Contribute STX to fund a prompt
- `close-prompt` - Creator can close their prompt

### Read-Only Functions

- `get-prompt` - Retrieve prompt details by ID
- `has-voted` - Check if a user has voted on a prompt
- `get-contribution` - Get contribution amount from a specific user
- `get-next-prompt-id` - Get the next available prompt ID

## Usage

Deploy the contract using Clarinet and interact through the Stacks blockchain. Users can propose creative AI prompt challenges, gather community support through voting, and receive funding to execute their ideas.