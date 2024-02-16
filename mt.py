import asyncio
import concurrent.futures

def waste_time(id, n):
    for i in range(n):
        pass

async def main():
    j = 100
    k = 1000000

    with concurrent.futures.ThreadPoolExecutor() as pool:
        for i in range(k):
            await asyncio.get_event_loop().run_in_executor(pool, waste_time, i, j)

loop = asyncio.get_event_loop()
loop.run_until_complete(main())
loop.close()