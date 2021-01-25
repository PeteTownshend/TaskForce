# taskForce

in general cli is working and provides
	history
	tab completion
	level switches
	report
description could be added afterwards
	
TODO: 
	shutdown on ctrl-c

> task <tag>
adds Task(tag, "", [(TimeStamp, Created)])
>

> tasks
prints list of tasks
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

> run <tag> 
	runs existing todo or creates a task
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