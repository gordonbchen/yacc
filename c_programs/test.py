import subprocess
from pathlib import Path


subprocess.run(["stack", "build"], capture_output=True)

fails = 0
success = 0
for cfile in Path(".").glob("*/*.c"):
	executable = str(cfile.parent / cfile.stem)
	subprocess.run(["gcc", str(cfile), "-o", executable], capture_output=True)
	gcc_ret = subprocess.run([f"./{executable}"], capture_output=True).returncode
	subprocess.run(["rm", executable])

	my_asm = str(cfile.parent / f"my_{cfile.stem}.s")
	stack_out = subprocess.run(["stack", "exec", "yacc-exe", "--", cfile, my_asm], capture_output=True).stdout
	subprocess.run(["gcc", my_asm, "-o", executable], capture_output=True)
	my_ret = subprocess.run([f"./{executable}"], capture_output=True).returncode
	subprocess.run(["rm", my_asm, executable])

	print(f"{str(cfile):<{32}} gcc {str(gcc_ret):<{10}} me {str(my_ret):<{10}}", end="")
	if my_ret == gcc_ret:
		success += 1
		print()
	else:
		print("FAIL")
		print(stack_out.decode())
		fails += 1

print(f"\n{fails} / {success + fails} failed")
