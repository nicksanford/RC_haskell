from aiohttp import web

async def handle(request):
    return web.Response(text=request.query_string)


def main():
    app = web.Application()
    app.add_routes([web.route('*', '/{tail:.*}', handle)])
    web.run_app(app)

if __name__ == '__main__':
    main()
