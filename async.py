import asyncio

async def waste_time(id, n):
    # print(id)
    for i in range(n):
        pass

async def main():
    j = 100
    k = 10000000

    foo = []
    for i in range(k):
        foo.append(waste_time(i, j))

    for f in foo:
        await f

asyncio.run(main())