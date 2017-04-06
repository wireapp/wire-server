# Wire™

[![Wire logo](https://github.com/wireapp/wire/blob/master/assets/header-small.png?raw=true)](https://wire.com/jobs/)

This repository is part of the source code of Wire. You can find more information at [wire.com](https://wire.com) or by contacting opensource@wire.com.

You can find the published source code at [github.com/wireapp/wire](https://github.com/wireapp/wire).

For licensing information, see the attached LICENSE file and the list of third-party licenses at [wire.com/legal/licenses/](https://wire.com/legal/licenses/).

No license is granted to the Wire trademark and its associated logos, all of which will continue to be owned exclusively by Wire Swiss GmbH. Any use of the Wire trademark and/or its associated logos is expressly prohibited without the express prior written consent of Wire Swiss GmbH.

## Wire server

This repository contains the source code for the Wire server. At the moment it contains a subset of the libraries and services used by Wire. We will keep releasing more libraries and services over time as we finish cleaning up and documenting the source code. Our goal is to publish the source code of the entire Wire server. 

## Content of the repository
This repository contains:

- **services**
   - **cannon**: handles communication with clients through websocket connections
   - **galley**: handles conversation creation and updates, connection requests and conversation participants
   - **nginz**: Wire's modules for nginx
   - **proxy**: proxies requests to third-party services so that clients do not need to connect to third-party directly
- **libs**: a collection of libraries used by various components for cross-service concerns (REST API, https, token generation, ...) 

## Roadmap
We are working on open sourcing the following components:

- Search and discovery of users
- Interaction with Apple's and Google's push services
- Routing of messages and events
- Asset storage and management
- Profile and devices management
