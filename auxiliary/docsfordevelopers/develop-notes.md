Cody added a test script - decide what to do with it:

I think it would be useful for repeated reviews of the apps. I wrote a script that reads the source files in the DSAIRMsolutions repo, parses the code chunks, evaluates the code, then compares the resulting "tasktable" to the complete solutions sheet. the script worked at least for the acute virus inputs. I tried to quickly convert it to a testthat script, but I am still unfamiliar with what is allowed in those and what is not. it currently fails. but I think this could be a good test, even if not for the package, it would be useful for manually debugging (this is how I usually review the apps, just automated instead of point-click manually).