Idea
====
Create an open source tool that allows users to create and share wish list. Collect as little user data as possible while allowing basic features:

 * Share the list by url, no account required to view it
 * Have special urls that allows the user to edit the wish list and add/remove wishes
 * Allow anonymous users (who still need to have the link) to mark wishes a completed/bought
 * Have an api that allows third-party clients to interact with the lists

Frontend
========
The example frontend is written in Elm. As of today (2022/11/15) it supports all basic features except editing wish lists and wish details.

Currently there is no prebuilt version available. The current [Docker image](https://hub.docker.com/repository/docker/b0wter/wishes-frontend) is hardcoded to connect to a test instance. This will be updated shortly.

Backend
=======
The backend is written in F# and uses ASP .Net Core with Giraffe. You can find a brief overview of the api [here](https://documenter.getpostman.com/view/1637871/2s8YKGjgSK).

The easiest way to run the backend is to use the image from [Docker Hub](https://hub.docker.com/repository/docker/b0wter/wishes-backend).
