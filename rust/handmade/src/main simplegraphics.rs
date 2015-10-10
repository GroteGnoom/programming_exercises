extern crate winapi;
extern crate user32;
extern crate kernel32;
extern crate gdi32;
extern crate libc;
extern crate time;
use std::ptr;
use std::mem;
use std::vec;
use time::SteadyTime;
use time::Duration;

static mut running : bool=true;
const defaultBITMAPINFOHEADER : winapi::BITMAPINFOHEADER = winapi::BITMAPINFOHEADER
{
    biSize: 0 as winapi::DWORD,
    biWidth: 0 as winapi::LONG,
    biHeight: 0 as winapi::LONG,
    biPlanes: 0 as winapi::WORD,
    biBitCount: 0 as winapi::WORD,
    biCompression: 0 as winapi::DWORD,
    biSizeImage: 0 as winapi::DWORD,
    biXPelsPerMeter: 0 as winapi::LONG,
    biYPelsPerMeter: 0 as winapi::LONG,
    biClrUsed: 0 as winapi::DWORD,
    biClrImportant: 0 as winapi::DWORD,
};

const defaultRGBQUAD : winapi::RGBQUAD =winapi::RGBQUAD
{
    rgbBlue: 0 as winapi::BYTE,
    rgbGreen:0 as winapi::BYTE,
    rgbRed:0 as winapi::BYTE,
    rgbReserved:0 as winapi::BYTE,
};



static mut defaultRECT : winapi::RECT = winapi::RECT
{
    left: 0,
    top: 0,
    right: 0,
    bottom: 0,
};

/*
static mut GlobalBackbuffer : win32_offscreen_buffer = win32_offscreen_buffer
{
     Info: defaultBITMAPINFO,
     Memory: 0.as_ptr() as *const libc::c_void, 
     Width: 0 as i32,
     Height: 0 as i32,
     Pitch: 0 as i32,
};
*/

#[allow(non_snake_case)]
unsafe extern "system" fn MainWindowCallback(Window: winapi::HWND, Message: winapi::UINT, WParam: winapi::WPARAM, LParam: winapi::LPARAM) -> i64
{
    let mut result = 0;
    //println!("Message: {:X}, WM_CLOSE: {:X}", Message, winapi::WM_CLOSE);
    match Message {
        winapi::WM_SIZE => println!("WM_SIZE"),//kernel32::OutputDebugStringA("WM_SIZE"),
        winapi::WM_DESTROY => 
        {
            println!("WM_DESTROY");
            running=false;   
        },
        winapi::WM_CLOSE => 
        {
            running = false;
            println!("running: {}", running);
        },
        winapi::WM_ACTIVATEAPP => println!("WM_ACTIVATEAPP"),
        winapi::WM_PAINT => {
            //let mut Paint: winapi::PAINTSTRUCT;
           let mut Paint = winapi::PAINTSTRUCT { //initialisation mandatory by rust
                hdc: ptr::null_mut(),
                fErase: 0 as libc::BOOL,
                rcPaint: winapi::RECT {
                    left: 0 as libc::LONG,
                    top: 0 as libc::LONG,
                    right: 0 as libc::LONG,
                    bottom: 0 as libc::LONG
                },
                fRestore: 0 as libc::BOOL,
                fIncUpdate: 0 as libc::BOOL,
                rgbReserved: [0 as libc::BYTE; 32],
            };
            Paint.rcPaint.left = 0;
            let DeviceContext = unsafe { user32::BeginPaint(Window, &mut Paint) };
            println! ("jaja: {:?}", Paint);
            let X = Paint.rcPaint.left;
            let Y = Paint.rcPaint.top;
            let Height = Paint.rcPaint.bottom -Paint.rcPaint.top;
            let Width = Paint.rcPaint.right -Paint.rcPaint.left;
            gdi32::PatBlt(DeviceContext, X,Y,Width,Height, 0);

        },
        _ => {//println!("default");
            result = user32::DefWindowProcW(Window, Message, WParam, LParam)
        }
    }
    result
}

struct win32_offscreen_buffer
{
     Info: winapi::BITMAPINFO,
     Memory: *const libc::c_void,
     Width: i32,
     Height: i32,
     Pitch: i32,
}
/*
unsafe fn
RenderWeirdGradient(Buffer: win32_offscreen_buffer, BlueOffset : i32, GreenOffset: i32)
{
    uint8 * Row = (uint8 *)Buffer->Memory;
    
    for (int Y=0;
            Y<Buffer->Height;
            ++Y)
    {
        uint32 *Pixel =(uint32 *)Row;
        for(int X=0;
            X< Buffer->Width;
            ++X)
        {
            uint8 Blue =(X+BlueOffset);
            uint8 Green =(Y+GreenOffset);


            *Pixel++ = ((Green << 8) | Blue);
            
        }
        Row += Buffer->Pitch;
    }
}
*/

/*
unsafe fn
Win32ResizeDIBSection(Buffer: win32_offscreen_buffer, Width: i32, Height: i32) //device independent bitmap
{
    let Align : i32 = 4;
    let BytesPerPixel : i32 =4;

    Buffer.Width=Width;
    Buffer.Height = Height;

    Buffer.Info.bmiHeader.biSize = mem::size_of::<winapi::BITMAPINFOHEADER>;
    Buffer.Info.bmiHeader.biWidth= Buffer.Width;
    Buffer.Info.bmiHeader.biHeight= -Buffer.Height;
    Buffer.Info.bmiHeader.biPlanes= 1;
    Buffer.Info.bmiHeader.biBitCount= 32;
    Buffer.Info.bmiHeader.biCompression = winapi::BI_RGB;

    let BitmapMemorySize: i32 = (Buffer.Width*Buffer.Height)*BytesPerPixel;
    //Buffer.Memory= winapi::VirtualAlloc (0, BitmapMemorySize, winapi::MEM_RESERVE|winapi::MEM_COMMIT, winapi::PAGE_READWRITE);
    let tempBuffer : [u8; 4*Buffer.Width*Buffer.Height] = [0;4*Buffer.Width*Buffer.Height];
    Buffer.Memory = tempBuffer;
    Buffer.Pitch = Width*BytesPerPixel;
}
*/
unsafe fn
Win32InitDIBSection(Buffer: &mut win32_offscreen_buffer) //device independent bitmap
{
    let Align : i32 = 4;
    let BytesPerPixel : i32 =4;

    Buffer.Info.bmiHeader.biSize = mem::size_of::<winapi::BITMAPINFOHEADER>() as u32;
    Buffer.Info.bmiHeader.biWidth= Buffer.Width;
    Buffer.Info.bmiHeader.biHeight= -Buffer.Height;
    Buffer.Info.bmiHeader.biPlanes= 1;
    Buffer.Info.bmiHeader.biBitCount= 32;
    Buffer.Info.bmiHeader.biCompression = winapi::BI_RGB;

    Buffer.Pitch = Buffer.Width*BytesPerPixel;
}


unsafe fn Win32DisplayBufferInWindow(Buffer: & win32_offscreen_buffer, DeviceContext: winapi::HDC, WindowWidth: i32, WindowHeight: i32)
{
    gdi32::StretchDIBits(
        DeviceContext,
        0,
        0,
        WindowWidth,
        WindowHeight,
        0,
        0,
        Buffer.Width,
        Buffer.Height,
        Buffer.Memory,
        &Buffer.Info,
        winapi::DIB_RGB_COLORS,
        13369376 as winapi::DWORD, //SRCCOPY

    );

}

struct win32_window_dimension
{
    Width: i32,
    Height: i32,
}

unsafe fn Win32GetWindowDimension (Window: winapi::HWND) -> win32_window_dimension
{
    //win32_window_dimension Result;

    let mut ClientRect: winapi::RECT = defaultRECT;
    user32::GetClientRect(Window, &mut ClientRect);

    let dimension = win32_window_dimension 
    {
        Width: ClientRect.right- ClientRect.left,
        Height: ClientRect.bottom- ClientRect.top,
    };

    return(dimension);
}
#[allow(non_snake_case)]
unsafe fn WinMain(Instance: winapi::HINSTANCE, PrevInstance: winapi::HINSTANCE, CommandLine: winapi::LPSTR, ShowCode: libc::LONG)
{
    let mut WindowClass = winapi::WNDCLASSW
    {
        style: winapi::CS_HREDRAW|winapi::CS_VREDRAW|winapi::CS_OWNDC,
        lpfnWndProc: Some(MainWindowCallback), 
        cbClsExtra: 0 as i32,
        cbWndExtra: 0 as i32,
        hInstance: Instance,
        hIcon: 0 as winapi::HICON,
        hCursor: 0 as winapi::HCURSOR,
        hbrBackground: 0 as winapi::HBRUSH,
        lpszMenuName: "MenuName".as_ptr() as *const u16,
        lpszClassName: "HandmadeHeroWindowClass".as_ptr() as *const u16,
    };
    if (user32::RegisterClassW(& WindowClass)!= 0 as u16) //RegisterClassA?
    {
        let Window: winapi::HWND = user32::CreateWindowExW(
            0,
            WindowClass.lpszClassName,
            "Handmade Hero".as_ptr() as *const u16,
            winapi::WS_OVERLAPPEDWINDOW|winapi::WS_VISIBLE,
            winapi::CW_USEDEFAULT,
            winapi::CW_USEDEFAULT,
            winapi::CW_USEDEFAULT,
            winapi::CW_USEDEFAULT,
            ptr::null_mut(),
            ptr::null_mut(),
            Instance,
            ptr::null_mut()                    
        );
        
        //loop 
        //for x in 0..1000
        let mut defaultBITMAPINFO : winapi::BITMAPINFO = winapi::BITMAPINFO
            {
                bmiHeader: defaultBITMAPINFOHEADER,
                bmiColors: [defaultRGBQUAD; 1],
            };
        //let mut Buffer: Box<[u8; 4*1288*728]> = Box::new([0;4*1288*728]);
        //let mut Buffer = vec![5u8; 4*1288*728];
        //let mut Buffer: Box<[u8; 10]> = Box::new([0;10]);
        //let mut Buffer = vec![5u8; 4*1288*728].into_boxed_slice();
        let mut Buffer = make_arr(4*1288*728);
        let ref mut Backbuffer : &mut win32_offscreen_buffer = &mut win32_offscreen_buffer
            {
                 Info: defaultBITMAPINFO,
                 //Memory: Box::new([0 as u8;1]).as_ptr() as *const libc::c_void, //Buffer.as_ptr() as *const libc::c_void, 
                 //Memory: Buffer.as_ptr() as *const libc::c_void, 
                 Memory: vec![5u8; 4*1288*728].into_boxed_slice().as_ptr() as *const libc::c_void,// as *const libc::c_void, 
                 Width: 1288 as i32,
                 Height: 728 as i32,
                 Pitch: 0 as i32,
            }; 
        let Align : i32 = 4;
        let BytesPerPixel : i32 =4;

        Backbuffer.Info.bmiHeader.biSize = mem::size_of::<winapi::BITMAPINFOHEADER>() as u32;
        Backbuffer.Info.bmiHeader.biWidth= Backbuffer.Width;
        Backbuffer.Info.bmiHeader.biHeight= -Backbuffer.Height;
        Backbuffer.Info.bmiHeader.biPlanes= 1;
        Backbuffer.Info.bmiHeader.biBitCount= 32;
        Backbuffer.Info.bmiHeader.biCompression = winapi::BI_RGB;

        Backbuffer.Pitch = Backbuffer.Width*BytesPerPixel;
        //Win32InitDIBSection(&mut Backbuffer); 
        let mut Message = winapi::MSG 
            {
                hwnd: ptr::null_mut(),
                message: 0 as winapi::UINT,
                wParam: 0 as winapi::WPARAM,
                lParam: 0 as winapi::LPARAM,
                time: 0 as winapi::DWORD,
                pt: winapi::POINT { x: 0 as winapi::LONG, y: 0 as winapi::LONG },
            };
        let mut check =0;
        let mut frames = 0;
        let a: u8 = 100;
        let start = SteadyTime::now();
        while SteadyTime::now()- start < Duration::seconds(1)//running
        {
            check = (check +1) % 255 ;
            println!("baa {:?}", check);
            //if ((check % 60)==0)
            //{
            //    frames=frames+1;
            //    println!("Time: {:?} , {:?}, {}",start, SteadyTime::now(), frames);
            //}
            //let mut Message: *mut winapi::MSG = ptr::null_mut();
            //
                
            //let MessageResult: libc::BOOL;
            //println!("HIER!");
            while (user32::PeekMessageW(&mut Message, ptr::null_mut(), 0, 0,winapi::PM_REMOVE)!=0) 
            {
                //user32::PeekMessageW(Message, ptr::null_mut(), 0, 0, winapi::PM_REMOVE);
                println!("GotMessage: {:?}", Message);
                //println!("HIER2");
                user32::TranslateMessage(&mut Message);
                user32::DispatchMessageW(&mut Message);
            };

            let DeviceContext : winapi::HDC= user32::GetDC(Window); 
            let Dimension: win32_window_dimension = Win32GetWindowDimension(Window);
            //let mut memref: [u8] = Buffer;
            let mut x = 0;
            while (x<4*1288*728)
            {
                Buffer[x]=a;//check as u8;//=[check; 4*1288*728];
                x = x + 1;
            }
            Backbuffer.Memory=Buffer.as_ptr() as *const libc::c_void;
            Win32DisplayBufferInWindow(Backbuffer, DeviceContext, Dimension.Width, Dimension.Height);
            user32::ReleaseDC(Window, DeviceContext);
        }
        
    }


}
/*
fn func() 
{
    let a=ptr::null_mut();
    let b= "hello world".as_ptr() as *const i8;
    let c= "hello".as_ptr() as *const i8;
    let d: u32 = 0;
    unsafe 
    {
        user32::MessageBoxA(a,b,c,d);
    }
}
*/
#[allow(non_snake_case)]
fn main () 
{
    let Instance: winapi::HINSTANCE = ptr::null_mut();
    let PrevInstance: winapi::HINSTANCE = ptr::null_mut();
    let CommandLine: winapi::LPSTR =ptr::null_mut();
    let ShowCode: libc::LONG =0;
    unsafe
    {
        WinMain(Instance, PrevInstance, CommandLine, ShowCode )
    }
}

use std::iter::repeat;
fn make_arr(len: usize) -> Box<[u8]> {
    repeat(0).take(len).collect::<Vec<u8>>().into_boxed_slice()
}
