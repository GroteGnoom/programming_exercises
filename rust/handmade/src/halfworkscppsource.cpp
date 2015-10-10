#include <windows.h>


LRESULT CALLBACK MainWindowCallback
(
    HWND   Window, //these are now renamed. Should not alter the function
    UINT   Message,
    WPARAM WParam,
    LPARAM LParam
)
{
    LRESULT Result =0;

    switch(Message)
    {
        case WM_SIZE:
        {
            OutputDebugStringA("WM_SIZE\n");
        } break;

        case WM_DESTROY:
        {
            OutputDebugStringA("WM_DESTROY\n");
        } break;

        case WM_CLOSE:
        {
            OutputDebugStringA("WM_CLOSE\n");
        } break;


        case WM_ACTIVATEAPP:
        {
            OutputDebugStringA("WM_ACTIVATEAPP\n");
        } break;

        case WM_PAINT:
        {
            PAINTSTRUCT Paint;
            HDC DeviceContext = BeginPaint (Window, &Paint);
            int X = Paint.rcPaint.left;
            int Y = Paint.rcPaint.top;
            int Height = Paint.rcPaint.bottom -Paint.rcPaint.top;
            int Width = Paint.rcPaint.right -Paint.rcPaint.left;
            static DWORD Operation = WHITENESS;            
            PatBlt(DeviceContext, X,Y,Width,Height,Operation);
            if (Operation ==WHITENESS)
            {
                Operation =BLACKNESS;
            }
            else
            {
                Operation = WHITENESS;
            }
            EndPaint(Window, &Paint);
        }

        default:
        {
            OutputDebugStringA("WM_DEFAULT\n");
            Result = DefWindowProc(Window, Message, WParam, LParam); 
        } break;
    }
    return (Result);
};

int CALLBACK WinMain
(
  HINSTANCE Instance, // handle to this ---these paramaters are also renamed, look at the type or the function call for to look up info
  HINSTANCE PrevInstance, //always zero, checked if it was a copy
  LPSTR     CommandLine, //command line inputs?
  int       ShowCode //where it's run normal ,minimized or maximized
)
{
    WNDCLASS WindowClass = {}; // sets all attributes to zero.
    WindowClass.style = CS_OWNDC|CS_HREDRAW|CS_VREDRAW;
    WindowClass.lpfnWndProc = MainWindowCallback;
    WindowClass.hInstance = Instance;
    // WindowClass.hIcon;
    WindowClass.lpszClassName = "HandmadeHeroWindowClass";
 
    if (RegisterClass(&WindowClass)) 
    {
        HWND WindowHandle = CreateWindowEx(
                0, 
                WindowClass.lpszClassName,  
                "Handmade Hero", 
                 WS_OVERLAPPEDWINDOW|WS_VISIBLE, 
                CW_USEDEFAULT, 
                CW_USEDEFAULT, 
                CW_USEDEFAULT, 
                CW_USEDEFAULT, 
                0, 
                0, 
                Instance, 
                0 
        ); 
        if (WindowHandle) 
        {
            for(;;) 
            {
                MSG Message;
                BOOL MessageResult = GetMessage (&Message,0,0,0);
                if(MessageResult>0)
                {
                    TranslateMessage(&Message);
                    DispatchMessage(&Message);
                }
                else
                {
                    break;
                }
            }
        }
        else
        {
            //itfailed
        }
       
    }
    else
    {
            //registerclass failed
    }


  return(0);

}
   


 
