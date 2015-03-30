# Git Cheat Sheet 

* status
  * inpsect the current status of all files in the local directory compared to the repository, ex. modified, untracked, new file.
* add
  * track new files and update changes in the project snapshot which would be committed to the repository. 
* commit
  * record the snapshot with user log message. Contents of the snapshot reflects the changes that would be made to the repository. 
* push
  * push the snapshot to the repository, where GitHub would merge changes to the project. 
  * specify destination: git push origin <dst> 
* pull
  * Update a local branch by its remote version in the repository, merge the changes into the local branch.
  * - git pull {repository} {remote branch name}:{local branch name}
* clone
  * download a copy of a Git repository.
  * - git clone {repository url} {optional: new name}
* config 
  * Some settings to customize the Git environment, including user name, email, editor, merge tools.
* remote
  * manage remote repository aliases.
  * - git remote add {alias} {url} : add a new repository



