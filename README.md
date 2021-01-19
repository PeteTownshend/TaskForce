# taskForce

in general cli is working providing
	history
	tab completion
	level switches
description could be added afterwards, currently just one word possible
	
TODO: 
	shutdown on ctrl-c
	creating report
	add TODO list

> todo <tag> <description> 
adds Task(tag, description, [(TimeStamp, Created)])
>

> todo <tag>
adds Task(tag, "", [(TimeStamp, Created)])
>

> todos
returns list of todos
>

> run <tag> 
	runs existing todo or creates a task
	prints "newly task ..."
	description is from Todo or not set yet
	Task(tag, description, [(TimeStamp, Created), (TimeStamp, Start)])
	run tag >

	run tag > set <description> 
	inserts description into running task
	run tag >

	run tag > description
	returns task description
	run tag >

	run tag > log <text>
	adds (TimeStamp, Log text) to history
	run tag >

	run tag > history
	returns the history of the running task
	run tag >	

	run tag > exit
	returns to root and adds (TimeStamp, End) to history
	>

> delete <tag>
removes task
prints "deleted" or "<tag> doesn't exist"
>

> report
prints full report on tasks
>

> quit
stores application state and stopps application
>
