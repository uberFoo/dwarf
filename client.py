import asyncio
import aiohttp
# import concurrent.futures

async def fetch_url(url):
    async with aiohttp.ClientSession() as session:
        async with session.get(url) as response:
            return await response.text()

async def main():
    k = 100

    tasks = []
    for i in range(k):
        tasks.append(asyncio.create_task(fetch_url("https://www.rust-lang.org")))

    responses = await asyncio.gather(*tasks)
    for response in responses:
        print(response)


asyncio.run(main())